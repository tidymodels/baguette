#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @import dplyr
#' @import hardhat
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

#' @keywords internal
"_PACKAGE"

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom generics var_imp
#' @export
generics::var_imp

## usethis namespace: end
NULL

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
