
mars_bagger <- function(rs, opt, control, extract, ...) {

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

  rs <- extractor(rs, extract)

  imps <- compute_imp(rs, mars_imp, control$var_imp)

  oob <- compute_oob(rs, control$oob)

  rs <-
    rs %>%
    replace_parsnip_terms() %>%
    mutate(model = map(model, axe_mars))


  list(model = select_rs(rs), oob  = oob, imp = imps)
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



mars_fit  <- function(split, spec, control = bag_control()) {
  ctrl <- parsnip::fit_control(catch = TRUE)

  dat <- rsample::analysis(split)
  # only na.fail is supported by earth::earth
  dat <- dat[complete.cases(dat),, drop = FALSE]

  if (control$sampling == "down") {
    dat <- down_sampler(dat)
  }

  mod <- parsnip::fit.model_spec(spec, .outcome ~ ., data = dat, control = ctrl)
  mod
}

mars_imp <- function(x) {
  imps <- earth::evimp(x$fit)
  imps <- imps[, "gcv"]
  x <- tibble::tibble(predictor = names(imps), importance = unname(imps))
  x <- x[x$importance > 0,]
  x
}


