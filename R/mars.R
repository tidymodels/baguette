
mars_bagger <- function(rs, control, ...) {

  opt <- rlang::dots_list(...)
  is_classif <- is.factor(rs$splits[[1]]$data$.outcome)
  mod_spec <- make_mars_spec(is_classif, opt)

  iter <- get_iterator(control)

  rs <-
    rs %>%
    dplyr::mutate(model = iter(
      fit_seed,
      splits,
      seed_fit,
      .fn = mars_fit,
      spec = mod_spec,
      control = control
    ))

  rs <- check_for_disaster(rs)

  rs <- filter_rs(rs)

  rs <- extractor(rs, control$extract)

  imps <- compute_imp(rs, mars_imp, control$var_imp)

  rs <-
    rs %>%
    replace_parsnip_terms()

  if (control$reduce) {
    rs <-
     rs %>%
      mutate(model = map(model, axe_mars))
  }

  list(model = rs, imp = imps)
}

axe_mars <- function(x) {
  x$fit <- butcher::axe_data(x$fit)
  x$fit <- butcher::axe_call(x$fit)
  x$fit <- butcher::axe_fitted(x$fit)
  x
}

make_mars_spec <- function(classif, opt) {
  opts <- join_args(model_defaults[["MARS"]], opt)
  if (classif) {
    mars_md <- "classification"
  } else {
    mars_md <- "regression"
  }
  mars_spec <-
    mars(
      mode = mars_md,
      num_terms = !!opts$nprune,
      prod_degree = !!opts$degree,
      prune_method = !!opts$pmethod
    )
  opts <- opts[!(names(opts) %in% c("pmethod", "nprune", "degree"))]
  if (length(opts) > 0) {
    mars_spec <- set_engine(mars_spec, engine = "earth", !!!opts)
  } else {
    mars_spec <- set_engine(mars_spec, engine = "earth")
  }
  mars_spec
}



mars_fit  <- function(split, spec, control = control_bag()) {
  ctrl <- parsnip::control_parsnip(catch = TRUE)

  dat <- rsample::analysis(split)
  # only na.fail is supported by earth::earth
  dat <- dat[complete.cases(dat),, drop = FALSE]
  if (any(names(dat) == ".weights")) {
    wts <- hardhat::importance_weights(dat$.weights)
    dat$.weights <- NULL
  } else {
    wts <- NULL
  }

  if (control$sampling == "down") {
    dat <- down_sampler(dat)
  }

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

mars_imp <- function(x) {
  # see issue 71
  rlang::check_installed("earth")
  cl <- rlang::call2("evimp", .ns = "earth", object = quote(x$fit))
  imps <- rlang::eval_tidy(cl)
  imps <- imps[, "gcv", drop = FALSE]
  x <- tibble::tibble(predictor = rownames(imps), importance = unname(imps[, "gcv"]))
  x <- x[x$importance > 0,]
  x
}


