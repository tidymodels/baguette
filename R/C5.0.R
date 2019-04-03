#' @importFrom C50 C5.0 C5.0Control C5imp
#' @importFrom rsample analysis
#' @importFrom purrr map map2 map_df
#' @importFrom tibble tibble
#' @importFrom parsnip decision_tree
#' @importFrom furrr future_map

c5_bagger <- function(rs, opt, var_imp, oob, extract, ...) {
   mod_spec <- make_c5_spec(opt)
  rs <-
    rs %>%
    dplyr::mutate(
      model = furrr::future_map(splits, c5_fit, spec = mod_spec),
      passed = !purrr::map_lgl(model, model_failure)
    )

  check_for_disaster(rs)

  rs <- rs %>% dplyr::filter(passed)
  num_mod <- nrow(rs)

  if (var_imp) {
    imps <-
      purrr::map_df(rs$model, c5_imp) %>%
      dplyr::group_by(predictor) %>%
      dplyr::summarize(
        importance = sum(importance)/num_mod,
        used = length(predictor)
      ) %>%
      dplyr::arrange(desc(importance))
  } else {
    imps <- NULL
  }

  if (!is.null(oob)) {
    oob <-
      purrr::map2_dfr(rs$model, rs$splits, oob_parsnip, met = oob) %>%
      dplyr::group_by(.metric) %>%
      dplyr::summarize(
        mean = mean(.estimate, na.rm = TRUE),
        stdev = sd(.estimate, na.rm = TRUE),
        n = sum(!is.na(.estimate))
      )
  } else {
    oob <- NULL
  }

  if (!is.null(extract)) {
    rs <-
      rs %>%
      dplyr::mutate(extras = map(model, extract, ...))
  }

  list(
    model = rs %>% dplyr::select(-splits, -id, -fit_seed, -passed),
    imp = imps,
    oob = oob
  )
}

make_c5_spec <- function(opt) {
  opts <- join_args(model_defaults[["C5.0"]], opt)
  c5_spec <-
    parsnip::decision_tree(
      mode = "classification",
      min_n = !!opts$minCases
    )

  opts <- opts[!(names(opts) %in% c("minCases"))]

  if (length(opts) > 0) {
    main_args <- list(trials = 1)

    if (any(names(opts) == "rules")) {
      main_args$rules <- opts$rules
      opts$rules <- NULL
    }
    if (any(names(opts) == "costs")) {
      main_args$costs <- opts$costs
      opts$costs <- NULL
    }
    if (length(main_args) == 0) {
      main_args <- NULL
    }
    if (length(opts) == 0) {
      c5_spec <- parsnip::set_engine(c5_spec, engine = "C5.0", !!!main_args)
    } else {
      # make a control object from any other arguments:
      c5_ctrl_call <- rlang::call2("C5.0Control", !!!opts, .ns = "C50")
      ctrl <- rlang::eval_tidy(c5_ctrl_call)
      c5_spec <-
        parsnip::set_engine(c5_spec, engine = "C5.0", !!!main_args, control = ctrl)
    }
  } else {
    c5_spec <- parsnip::set_engine(c5_spec, engine = "C5.0")
  }
  c5_spec
}

#' @importFrom stats complete.cases

c5_fit  <- function(split, spec) {
  ctrl <- parsnip::fit_control(catch = TRUE)
  mod <-
    parsnip::fit.model_spec(spec,
                            .outcome ~ .,
                            data = rsample::analysis(split),
                            control = ctrl)
  mod
}

c5_imp <- function(x) {
  res <- C50::C5imp(x$fit)
  res <-
    tibble::tibble(
      predictor = rownames(res),
      importance = res$Overall
    )
  res <- res[res$importance > 0,]
  res
}

