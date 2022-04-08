library(rlang)
library(purrr)
library(yardstick)
library(recipes)

data("two_class_dat", package = "modeldata")

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
