

bagger_bridge <- function(processed, model, seed, B, opt, control, extract, ...) {
  validate_outcomes_is_univariate(processed$outcomes)
  if (model %in% c("C5.0")) {
    validate_outcomes_are_factors(processed$outcomes)
  }

  dat <- as.data.frame(processed$predictors)
  dat$.outcome <- processed$outcomes[[1]]

  set.seed(seed)
  rs <- rsample::bootstraps(dat, times = B) %>%
    dplyr::mutate(fit_seed = sample.int(10^5, B))

  res <- switch(
    model,
    model_rules = cubist_bagger(rs, opt, control, extract, ...),
    CART = cart_bagger(rs, opt, control, extract, ...),
    C5.0 = c5_bagger(rs, opt, control, extract, ...),
    MARS = mars_bagger(rs, opt, control, extract, ...)
  )

  res <-
    new_bagger(
      model_df = res$model,
      imp = res$imp,
      oob = res$oob,
      control = control,
      opt = opt,
      model = model,
      blueprint = processed$blueprint
    )
  res
}
