join_args <- function(default, others) {
  res <- default
  for (i in seq_along(others)) {
    res[[ names(others)[i] ]] <- others[[i]]
  }
  res
}

# ------------------------------------------------------------------------------

failed_stats <- tibble(.metric = "failed", .estiamtor = "none", .estimate = NA_real_)

#' @importFrom rsample assessment
oob_parsnip <- function(model, split, met) {
  dat <- rsample::assessment(split)
  y <- dat$.outcome
  dat <- dat[, names(dat) != ".outcome", drop = FALSE]
  pred <-
    predict(model, dat) %>%
    dplyr::mutate(.obs = y)
  if (model$spec$mode == "classification") {
    probs <- predict(model, dat, type = "prob")
    lvls <- names(probs)
    pred <- dplyr::bind_cols(pred, probs)
    res <- try(met(pred, truth = .obs, !!!lvls, estimate = .pred_class), silent = TRUE)
  } else {
    res <- try(met(pred, .obs, .pred), silent = TRUE)
  }
  if (inherits(res, "try-error")) {
    res <- failed_stats
  }
  res
}


