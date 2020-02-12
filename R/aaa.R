#' @import rlang
#' @import dplyr
#' @import hardhat
#'
#' @importFrom parsnip set_engine fit fit_xy fit_control mars decision_tree
#' @importFrom utils globalVariables
#' @importFrom earth earth evimp
#' @importFrom rsample analysis bootstraps assessment
#' @importFrom purrr map map2 map_df map_dfr map_lgl
#' @importFrom tibble tibble as_tibble is_tibble
#' @importFrom furrr future_map future_map2
#' @importFrom stats setNames sd predict complete.cases
#' @importFrom C50 C5.0 C5.0Control C5imp as.party.C5.0
#' @importFrom rpart rpart
#' @importFrom partykit as.party.rpart
#' @importFrom Cubist cubist cubistControl
#' @importFrom withr with_seed

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
    ".outcome"
  )
)
