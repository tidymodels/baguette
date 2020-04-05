bagger_bridge <- function(processed, base_model, seed, times, opt, control, cost, extract, ...) {
  validate_outcomes_are_univariate(processed$outcomes)
  if (base_model %in% c("C5.0")) {
    validate_outcomes_are_factors(processed$outcomes)
  }

  dat <- as.data.frame(processed$predictors)
  dat$.outcome <- processed$outcomes[[1]]

  set.seed(seed)
  rs <- rsample::bootstraps(dat, times = times) %>%
    dplyr::mutate(fit_seed = sample.int(10^5, times))

  if (is.null(cost)) {
    res <- switch(
      base_model,
      CART = cart_bagger(rs, opt, control, extract, ...),
      C5.0 =   c5_bagger(rs, opt, control, extract, ...),
      MARS = mars_bagger(rs, opt, control, extract, ...)
    )
  } else {
    res <- switch(
      base_model,
      CART = cost_sens_cart_bagger(rs, opt, control, cost, extract, ...),
      C5.0 =   cost_sens_c5_bagger(rs, opt, control, cost, extract, ...)
    )
  }

  res <-
    new_bagger(
      model_df = res$model,
      imp = res$imp,
      oob = NULL,
      control = control,
      cost = cost,
      opt = opt,
      base_model = base_model,
      blueprint = processed$blueprint
    )
  res
}
