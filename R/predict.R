#' Predictions from a bagged model
#'
#' The `predict()` function computes predictions from each of the
#'  models in the ensembles and returns a single aggregated value
#'  for each sample in `new_data`.
#' @param object An object generated by `bagger()`.
#' @param new_data A data frame of predictors. If a recipe or
#'  formula were originally used, the **original** data should be
#'  passed here instead of a preprocessed version.
#' @param type A single character value for the type of
#'  predictions. For regression models, `type = 'numeric'` is valid
#'  and `'class'` and `'prob'` are valid for classification models.
#' @param ... Not currently used.
#' @examples
#' data(airquality)
#'
#' set.seed(7687)
#' cart_bag <- bagger(Ozone ~ ., data = airquality, base_model = "CART", times = 5)
#' predict(cart_bag, new_data = airquality[, -1])
#' @export
predict.bagger <- function(object, new_data, type = NULL, ...) {
  type <- check_type(object, type)
  new_data <- hardhat::forge(new_data, object$blueprint)$predictors

  if (type == "numeric") {
    res <- numeric_pred(object$model_df, new_data)
  } else {
    lvl <- levels(object$blueprint$ptypes$outcomes[[1]])
    use_majority <- object$base_model[1] == "C5.0" && !is.null(object$cost)
    res <- class_pred_path(object$model_df, new_data, lvl, type, use_majority)
  }
  res
}


# For some cost-sensitive classification models, class probabilities cannot be
# naturally estimated. In these cases, the hard class predictions are
# summarized to use majority vote for the probability estimates.
class_pred_path <- function(models, data, lvl, type, majority = FALSE) {
  if (majority) {
    res <- majority_vote(models, data, lvl, type == "prob")
  } else {
    if (type == "class") {
      res <- class_pred(models, data, lvl)
    } else {
      res <- classprob_pred(models, data, lvl)
    }
  }
  res
}

# ------------------------------------------------------------------------------

numeric_pred <- function(models, data) {
  n <- nrow(data)
  m <- nrow(models)
  preds <-
    purrr::map_df(models$model, predict, new_data = data, type = "numeric") |>
    dplyr::mutate(.row = rep(1:n, m)) |>
    dplyr::group_by(.row) |>
    dplyr::summarize(
      # .n = sum(!is.na(.pred)),
      .pred = mean(.pred, na.rm = TRUE)
    ) |>
    dplyr::arrange(.row) |>
    dplyr::select(.pred)
  preds
}

class_pred <- function(models, data, lvl) {
  classprob_pred(models, data, lvl) |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    tidyr::pivot_longer(
      cols = c(dplyr::starts_with(".pred")),
      names_to = "class",
      values_to = "prob"
    ) |>
    dplyr::group_by(.row) |>
    dplyr::arrange(desc(prob)) |>
    dplyr::slice(1) |>
    dplyr::mutate(
      .pred_class = gsub(".pred_", "", class, fixed = TRUE),
      .pred_class = factor(.pred_class, levels = lvl)
    ) |>
    dplyr::ungroup() |>
    dplyr::select(.pred_class)
}

classprob_pred <- function(models, data, lvl) {
  n <- nrow(data)
  m <- nrow(models)
  prob_cols <- paste0(".pred_", lvl)
  preds <-
    purrr::map_df(models$model, predict, new_data = data, type = "prob") |>
    dplyr::mutate(.row = rep(1:n, m)) |>
    dplyr::group_by(.row) |>
    ## TODO normalize these; use pivot longer
    dplyr::summarize_at(prob_cols, mean, na.rm = TRUE) |>
    dplyr::arrange(.row) |>
    dplyr::select(-.row)
  preds
}

majority_vote <- function(models, data, lvl, probs = FALSE) {
  n <- nrow(data)
  m <- nrow(models)
  prob_cols <- paste0(".pred_", lvl)
  preds <-
    purrr::map_df(models$model, predict, new_data = data, type = "class") |>
    dplyr::mutate(.row = rep(1:n, each = m)) |>
    dplyr::group_by(.row) |>
    dplyr::count(.pred_class, .drop = FALSE) |>
    dplyr::mutate(n = n/m)

  if (probs) {
    preds <-
      preds |>
      dplyr::mutate(.pred_class = paste0(".pred_", .pred_class)) |>
      dplyr::ungroup() |>
      tidyr::pivot_wider(id_cols = .row,
                         names_from = ".pred_class",
                         values_from = "n") |>
      dplyr::select(dplyr::one_of(prob_cols))
  } else {
    preds <-
      preds |>
      dplyr::arrange(desc(n), .by_group = TRUE) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::select(.pred_class)
  }
  preds
}



