bagger_bridge <- function(processed, weights, base_model, seed, times, control, cost, ...) {
  validate_outcomes_are_univariate(processed$outcomes)
  if (base_model %in% c("C5.0")) {
    validate_outcomes_are_factors(processed$outcomes)
  }

  # TODO no cost for nnet
  dat <- as.data.frame(processed$predictors)
  validate_case_weights(weights, processed$predictors)

  dat$.outcome <- processed$outcomes[[1]]
  if (!is.null(weights)) {
    dat$.weights <- weights
  }

  set.seed(seed)
  rs <- rsample::bootstraps(dat, times = times) %>%
    dplyr::mutate(fit_seed = sample.int(10^5, times))

  if (is.null(cost)) {
    res <- switch(
      base_model,
      CART = cart_bagger(rs, control, ...),
      C5.0 =   c5_bagger(rs, control, ...),
      MARS = mars_bagger(rs, control, ...),
      nnet = nnet_bagger(rs, control, ...),
    )
  } else {
    res <- switch(
      base_model,
      CART = cost_sens_cart_bagger(rs, control, cost, ...),
      C5.0 =   cost_sens_c5_bagger(rs, control, cost, ...)
    )
  }

  res <-
    new_bagger(
      model_df = res$model,
      imp = res$imp,
      control = control,
      cost = cost,
      base_model = base_model,
      blueprint = processed$blueprint
    )
  res
}

validate_case_weights <- function(weights, data) {
  if (is.null(weights)) {
    return(invisible(NULL))
  }
  n <- nrow(data)
  if (!is.vector(weights) || !is.numeric(weights) || length(weights) != n ||
      any(weights < 0)) {
    rlang::abort("'weights' should be a non-negative numeric vector with the same size as the data.")
  }
  invisible(NULL)
}

