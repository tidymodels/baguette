#' @import rlang
#' @import dplyr
#' @import hardhat
#' @importFrom parsnip set_engine fit fit_xy fit_control
#' @importFrom utils globalVariables

# ------------------------------------------------------------------------------

utils::globalVariables(
  c(
    "fit_seed",
    "id",
    "importance",
    "model",
    "passed",
    "predictor",
    "splits",
    ".estimate",
    ".metric",
    ".obs",
    ".pred",
    ".pred_class",
    "mod"
  )
)
