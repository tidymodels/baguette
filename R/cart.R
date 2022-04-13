
cart_bagger <- function(rs, control, ...) {
  opt <- rlang::dots_list(...)
  is_classif <- is.factor(rs$splits[[1]]$data$.outcome)
  mod_spec <- make_cart_spec(is_classif, opt)

  iter <- get_iterator(control)

  rs <-
    rs %>%
    dplyr::mutate(
      model = iter(
        fit_seed,
        splits,
        seed_fit,
        .fn = cart_fit,
        spec = mod_spec,
        control = control
      )
    )

  rs <- check_for_disaster(rs)

  rs <- filter_rs(rs)

  rs <- extractor(rs, control$extract)

  imps <- compute_imp(rs, cart_imp, control$var_imp)

  rs <-
    rs %>%
    replace_parsnip_terms()

  if (control$reduce) {
    rs <-
      rs %>%
      mutate(model = map(model, axe_cart))
  }

  list(model = rs, imp = imps)
}

make_cart_spec <- function(classif, opt) {
  opts <- join_args(model_defaults[["CART"]], opt)
  if (classif) {
    cart_md <- "classification"
  } else {
    cart_md <- "regression"
  }
  cart_spec <-
    parsnip::decision_tree(
      mode = cart_md,
      cost_complexity = !!opts$cp,
      min_n = !!opts$minsplit,
      tree_depth = !!opts$maxdepth
    )

  opts <- opts[!(names(opts) %in% c("cp", "maxdepth", "minsplit"))]

  if (length(opts) > 0) {
    main_args <- list()

    if (any(names(opts) == "method")) {
      main_args$method <- opts$method
      opts$method <- NULL
    }
    if (any(names(opts) == "parms")) {
      main_args$parms <- opts$parms
      opts$parms <- NULL
    }
    if (any(names(opts) == "cost")) {
      main_args$cost <- opts$cost
      opts$cost <- NULL
    }
    if (length(opts) == 0) {
      opts <- NULL
    }
    if (length(main_args) == 0) {
      main_args <- NULL
    }

    # Note: from ?rpart: "arguments to rpartcontrol may also be specified in
    # the call to rpart. They are checked against the list of valid arguments."

    cart_spec <-
      parsnip::set_engine(cart_spec, engine = "rpart", !!!main_args, !!!opts)
  } else {
    cart_spec <- parsnip::set_engine(cart_spec, engine = "rpart")
  }
  cart_spec
}


cart_fit  <- function(split, spec, control = control_bag()) {

  dat <- rsample::analysis(split)

  if (control$sampling == "down") {
    dat <- down_sampler(dat)
  }
  if (any(names(dat) == ".weights")) {
    wts <- hardhat::importance_weights(dat$.weights)
    dat$.weights <- NULL
  } else {
    wts <- NULL
  }

  ctrl <- parsnip::fit_control(catch = TRUE)
  mod <-
    parsnip::fit.model_spec(
      spec,
      .outcome ~ .,
      data = dat,
      control = ctrl,
      case_weights = wts
    )
  mod
}

cart_imp <- function(x) {
  if (!any(names(x$fit) == "variable.importance")) {
    x <-
      tibble::tibble(
        predictor = rlang::na_chr,
        importance = rlang::na_dbl
      )
  } else {
    x <-
      tibble::tibble(
        predictor = names(x$fit$variable.importance),
        importance = unname(x$fit$variable.importance)
      )
    x <- x[x$importance > 0,]
  }
  x
}

axe_cart <- function(x) {
  x$fit <- butcher::axe_data(x$fit)
  x$fit <- butcher::axe_ctrl(x$fit)
  x$fit <- butcher::axe_call(x$fit)
  x$fit <- butcher::axe_env(x$fit)
  x
}
