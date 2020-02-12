join_args <- function(default, others) {
  res <- default
  for (i in seq_along(others)) {
    res[[ names(others)[i] ]] <- others[[i]]
  }
  res
}

# ------------------------------------------------------------------------------

failed_stats <- tibble(.metric = "failed", .estiamtor = "none", .estimate = NA_real_)

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

oob_cubist <- function(model, split, met) {
  dat <- rsample::assessment(split)
  y <- dat$.outcome
  dat <- dat[, names(dat) != ".outcome", drop = FALSE]
  pred <-
    tibble(.pred = predict(model, dat)) %>%
    dplyr::mutate(.obs = y)

  res <- try(met(pred, .obs, .pred), silent = TRUE)

  if (inherits(res, "try-error")) {
    res <- failed_stats
  }
  res
}


compute_oob <- function(rs, oob) {

  if (inherits(rs$model[[1]], "cubist")) {
    .fn <- oob_cubist
  } else {
    .fn <- oob_parsnip
  }

  if (!is.null(oob)) {
    oob <-
      purrr::map2_dfr(rs$model, rs$splits, .fn, met = oob) %>%
      dplyr::group_by(.metric) %>%
      dplyr::summarize(
        mean = mean(.estimate, na.rm = TRUE),
        stdev = sd(.estimate, na.rm = TRUE),
        n = sum(!is.na(.estimate))
      )
  } else {
    oob <- NULL
  }
  oob
}

# ------------------------------------------------------------------------------

compute_imp <- function(rs, .fn, compute) {
  if (compute) {
    num_mod <- nrow(rs)
    imps <-
      purrr::map_df(rs$model, .fn) %>%
      dplyr::group_by(predictor) %>%
      dplyr::summarize(
        value = sum(importance, na.rm = TRUE)/num_mod,
        sds = sd(importance, na.rm = TRUE),
        sds = ifelse(length(predictor) == 1, 0, sds),
        std.error = sds/sqrt(num_mod),
        used = length(predictor)
      ) %>%
      dplyr::select(-sds) %>%
      dplyr::arrange(desc(value)) %>%
      dplyr::rename(term = predictor)
  } else {
    imps <- NULL
  }
  validate_importance(imps)
  imps
}

# ------------------------------------------------------------------------------

extractor <- function(rs, extract) {
  if (!is.null(extract)) {
    rs <- rs %>% dplyr::mutate(extras = map(model, ~ extract(.x$fit, ...)))
  }
  rs
}

# ------------------------------------------------------------------------------

select_rs <- function(rs) {
  rs %>% dplyr::select(-splits, -id, -fit_seed, -passed)
}

filter_rs <- function(rs) {
  if (any(names(rs) == "passed")) {
    rs <- rs %>% dplyr::filter(passed)
  }
  rs
}

# ------------------------------------------------------------------------------


seed_fit <- function(seed, split, .fn, ...) {
  withr::with_seed(seed, .fn(split, ...))
}

# ------------------------------------------------------------------------------

down_sampler <- function(x) {

  if (!is.factor(x$.outcome)) {
    rlang::warn("Down-sampling is only used in classification models.", call. = FALSE)
    return(x)
  }

  min_n <- min(table(x$.outcome))
  x %>%
    group_by(.outcome) %>%
    sample_n(size = min_n, replace = TRUE) %>%
    ungroup()
}



# ------------------------------------------------------------------------------

get_iterator <- function(control) {
  if (control$allow_parallel) {
    iter <- furrr::future_map2
  } else {
    iter <- purrr::map2
  }
  iter
}

# ------------------------------------------------------------------------------

replaced <- function(x, replacement) {
  x$preproc$terms <- replacement
  x
}

replace_parsnip_terms <- function(x) {
  new_terms <- butcher::axe_env(x$model[[1]]$preproc$terms)
  x <- x %>%
    mutate(model = map(model, replaced, replacement = new_terms))
  x
}


