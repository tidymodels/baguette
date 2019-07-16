#' @importFrom C50 C5.0 C5.0Control C5imp as.party.C5.0
#' @importFrom rsample analysis
#' @importFrom purrr map map2 map_df
#' @importFrom tibble tibble
#' @importFrom parsnip decision_tree
#' @importFrom furrr future_map

c5_bagger <- function(rs, opt, control, extract, ...) {

  mod_spec <- make_c5_spec(opt)
  rs <-
    rs %>%
    dplyr::mutate(model = furrr::future_map2(fit_seed, splits, seed_fit,
                                             .fn = c5_fit, spec = mod_spec))

  rs <- check_for_disaster(rs)

  rs <- filter_rs(rs)

  rs <- extractor(rs, extract)

  imps <- compute_imp(rs, c5_imp, control$var_imp)

  oob <- compute_oob(rs, control$oob)

  rs <-
    rs %>%
    mutate(
      model = map(model, ~ C50::as.party.C5.0(.x$fit)),
      .pred_form = map(model, tidypredict::tidypredict_fit)
    )

  list(model = select_rs(rs), oob  = oob, imp = imps)
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


