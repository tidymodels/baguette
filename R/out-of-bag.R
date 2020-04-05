failed_stats <- tibble(.metric = "failed", .estimator = "none", .estimate = NA_real_)

oob_parsnip <- function(model, split, met) {
  dat <- rsample::assessment(split)
  y <- dat$.outcome
  dat <- dat[, names(dat) != ".outcome", drop = FALSE]
  pred <-
    predict(model, dat) %>%
    dplyr::mutate(.obs = y)
  if (model$spec$mode == "classification") {
    probs <- predict(model, dat, type = "prob")
    probs <- setNames(probs, gsub(".pred_", "", names(probs)))

    lvls <- names(probs)

    if (length(lvls) == 2) {
      lvl_1 <- getOption("yardstick.event_first")
      if (lvl_1) {
        probs <- probs[, -2, drop = FALSE]
        lvls <- lvls[-2]
      } else {
        probs <- probs[, -1, drop = FALSE]
        lvls <- lvls[-1]
      }
      pred <- dplyr::bind_cols(pred, probs)
      res <- try(met(pred, truth = .obs, !!!lvls, estimate = .pred_class), silent = TRUE)
    } else {
      pred <- dplyr::bind_cols(pred, probs)
      res <- try(met(pred, truth = .obs, !!!lvls, estimate = .pred_class), silent = TRUE)
    }


  } else {
    res <- try(met(pred, .obs, .pred), silent = TRUE)
  }
  if (inherits(res, "try-error")) {
    res <- failed_stats
  }
  res
}

compute_oob <- function(rs, oob) {

  .fn <- oob_parsnip

  if (!is.null(oob)) {
    oob <-
      purrr::map2_dfr(rs$model, rs$splits, .fn, met = oob) %>%
      dplyr::group_by(.metric, .estimator) %>%
      dplyr::summarize(.estimate = mean(.estimate, na.rm = TRUE)) %>%
      mutate(.estimator = "out-of-bag") %>%
      dplyr::ungroup()
  } else {
    oob <- NULL
  }
  oob
}

