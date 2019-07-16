#' @importFrom earth earth evimp
#' @importFrom rsample analysis
#' @importFrom purrr map map2 map_df
#' @importFrom tibble tibble
#' @importFrom parsnip mars
#' @importFrom furrr future_map

mars_bagger <- function(rs, opt, var_imp, oob, extract, ...) {

  is_classif <- is.factor(rs$splits[[1]]$data$.outcome)
  mod_spec <- make_mars_spec(is_classif, opt)

  rs <-
    rs %>%
    dplyr::mutate(model = furrr::future_map(splits, mars_fit, spec = mod_spec))

  rs <- check_for_disaster(rs)

  rs <- filter_rs(rs)

  rs <- extractor(rs, extract)

  imps <- compute_imp(rs, mars_imp, var_imp)

  oob <- compute_oob(rs, oob)

  rs <- rs %>% mutate(.pred_form = map(model, tidypredict::tidypredict_fit))

  list(model = select_rs(rs), oob  = oob, imp = imps)
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

#' @importFrom stats complete.cases

mars_fit  <- function(split, spec) {
  ctrl <- parsnip::fit_control(catch = TRUE)
  dat <- rsample::analysis(split)
  # only na.fail is supported by earth::earth
  dat <- dat[complete.cases(dat),, drop = FALSE]
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


