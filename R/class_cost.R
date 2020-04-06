#' Cost parameter for minority class
#'
#' Used in `bag_treer()`.
#'
#' @param range A two-element vector holding the _defaults_ for the smallest and
#' largest possible values, respectively.
#'
#' @param trans A `trans` object from the `scales` package, such as
#' `scales::log10_trans()` or `scales::reciprocal_trans()`. If not provided,
#' the default is used which matches the units used in `range`. If no
#' transformation, `NULL`.
#'
#' @details
#' This parameter reflects the cost of a mis-classified sample relative to a
#' baseline cost of 1.0. For example, if the first level of an outcome factor
#' occurred rarely, it might help if this parameter were set to values greater
#' than 1.0. If the second level of the outcome factor is in the minority,
#' values less than 1.0 would cause the model to emphasize the minority class
#' more than the majority class.
#' @examples
#' class_cost()
#' @export
class_cost <- function(range = c(0, 5), trans = NULL) {
  dials::new_quant_param(
    type = "double",
    range = range,
    inclusive = c(TRUE, TRUE),
    trans = trans,
    label = c(class_cost = "Class Cost")
  )
}
