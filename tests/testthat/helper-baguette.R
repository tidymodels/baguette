library(rlang)
library(purrr)
if (is_installed("yardstick")) {
  library(yardstick)
}
if (is_installed("recipes")) {
  library(recipes)
}
if (is_installed("modeldata")) {
  data("two_class_dat", package = "modeldata")
}

# ------------------------------------------------------------------------------

num_leaves <- function(x, ...) {
  sum(x$frame$var == "<leaf>")
}

get_method <- function(x, ...) {
  x$method
}

get_loss <- function(x, ...) {
  x$parms$loss
}
