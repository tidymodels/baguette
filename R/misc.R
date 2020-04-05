join_args <- function(default, others) {
  res <- default
  for (i in seq_along(others)) {
    res[[ names(others)[i] ]] <- others[[i]]
  }
  res
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

# ------------------------------------------------------------------------------

# fix column names (see https://github.com/tidymodels/parsnip/issues/263)

fix_column_names <- function(result, object) {
  # print("# ------------------------------------------------------------------------------\n")
  # print(head(result))
  nms <- colnames(result)
  nms <- gsub(".pred_", "", nms, fixed = TRUE)
  result <- setNames(result, nms)
  # print(head(result))
  result
}

