
c5_bagger <- function(rs, opt, control, extract, ...) {

  mod_spec <- make_c5_spec(opt)

  iter <- get_iterator(control)

  rs <-
    rs %>%
    dplyr::mutate(model = iter(
      fit_seed,
      splits,
      seed_fit,
      .fn = c5_fit,
      spec = mod_spec,
      control = control
    ))

  rs <- check_for_disaster(rs)

  rs <- filter_rs(rs)

  rs <- extractor(rs, extract)

  imps <- compute_imp(rs, c5_imp, control$var_imp)

  rs <-
    rs %>%
    replace_parsnip_terms()

  if (control$reduce) {
    rs <-
      rs %>%
      mutate(model = map(model, axe_C5))
  }

  list(model = rs, imp = imps)
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


c5_fit  <- function(split, spec, control = control_bag()) {
  ctrl <- parsnip::fit_control(catch = TRUE)

  dat <- rsample::analysis(split)

  if (control$sampling == "down") {
    dat <- down_sampler(dat)
  }

  mod <- parsnip::fit.model_spec(spec, .outcome ~ ., data = dat, control = ctrl)
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


axe_C5 <- function(x) {
  x$fit <- butcher::axe_data(x$fit)
  x$fit <- butcher::axe_ctrl(x$fit)
  x$fit <- butcher::axe_call(x$fit)
  x$fit <- butcher::axe_fitted(x$fit)
  x
}

