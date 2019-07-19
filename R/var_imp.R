#' Obtain variable importance scores
#'
#' @param object An object.
#' @param ... Not currently used.
#' @return A tibble with columns for `term` (the predictor), `value` (the
#'  mean importance score), `std.error` (the standard error), and `used` (the
#'  occurrences of the predictors).
#' @details `baguette` can compute different variable importance scores for
#'  each model in the ensemble. The `var_imp()` function returns the average
#'  importance score for each model. Additionally, the function returns the
#'  number of times that each predictor is included in the final prediction
#'  equation.
#'
#' Specific methods used by the models are:
#'
#' _CART_: The model accumulates the improvement of the model that occurs when
#'  a predictor is used in a split. These values are taken form the `rpart`
#'  object. See `rpart::rpart.object()`.
#'
#' _MARS_: MARS models include a backwards elimination feature selection
#'  routine that looks at reductions in the generalized cross-validation (GCV)
#'  estimate of error. The `earth()` function tracks the changes in model
#'  statistics, such as the GCV, for each predictor and accumulates the
#'  reduction in the statistic when each predictor's feature is added to the
#'  model. This total reduction is used as the variable importance measure. If a
#'  predictor was never used in any of the MARS basis functions in the final
#'  model (after pruning), it has an importance value of zero. `baguette` wraps
#'  `earth::evimp()`.
#'
#' _Model Rules_: The underlying Cubist model calculates the occurrence of
#'  predictors inside the model rule definitions as well as the linear
#'  regression models contained within each rule. `baguette` wraps
#'  `Cubist::summary.cubist()`.
#'
#' _C5.0_: `C5.0` measures predictor importance by determining the percentage
#'  of training set samples that fall into all the terminal nodes after the
#'  split. For example, the predictor in the first split automatically has an
#'  importance measurement of 100 percent since all samples are affected by this
#'  split. Other predictors may be used frequently in splits, but if the
#'  terminal nodes cover only a handful of training set samples, the importance
#'  scores may be close to zero.
#'
#' Note that the `value` column that is the average of the importance scores
#'  form each model. The divisor of this average (and the corresponding standard
#'  error) is the number of models (as opposed to the number of models that
#'  used the predictor). This means that the importance scores for a predictor
#'  that was not used in the model has an implicit zero importance.
#' @export
#' @export var_imp.bagger
var_imp.bagger <- function(object, ...) {
  object$imp

}

