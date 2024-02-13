#' @import rlang
#' @import dplyr
#' @import hardhat
#'
#' @importFrom parsnip set_engine fit fit_xy control_parsnip mars decision_tree
#' @importFrom parsnip set_new_model multi_predict update_dot_check show_fit
#' @importFrom parsnip new_model_spec null_value update_main_parameters
#' @importFrom parsnip check_final_param model_printer show_call
#' @importFrom utils globalVariables
#' @importFrom rsample analysis bootstraps assessment
#' @importFrom purrr map map2 map_df map_dfr map_lgl
#' @importFrom tibble tibble as_tibble is_tibble
#' @importFrom furrr future_map future_map2
#' @importFrom stats setNames sd predict complete.cases
#' @importFrom C50 C5.0 C5.0Control C5imp as.party.C5.0
#' @importFrom rpart rpart
#' @importFrom withr with_seed
#' @importFrom dials new_quant_param
#' @importFrom stats coef
#'
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
    "mod",
    "value",
    ".outcome",
    "prob",
    "sds",
    ".estimator"
  )
)

# ------------------------------------------------------------------------------

# The functions below define the model information. These access the model
# environment inside of parsnip so they have to be executed once parsnip has
# been loaded.

.onLoad <- function(libname, pkgname) {
  # This defines model functions in the parsnip model database
  make_bag_tree()
  make_bag_mars()
  make_bag_mlp()
}
