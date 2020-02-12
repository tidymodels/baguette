library(testthat)
library(rlang)

context("MARS models")

# ------------------------------------------------------------------------------

data("two_class_dat", package = "rsample")

# ------------------------------------------------------------------------------

test_that('check mars opt', {

  check_pruning <- function(x, ...) {
    rlang::eval_tidy(x$call$pmethod) == "backward"
  }
  mod_1 <-
    bagger(
      mpg ~ .,
      data = mtcars,
      model = "MARS",
      pmethod = "backward",
      control = bag_control(var_imp = FALSE),
      extract = check_pruning
    )
  expect_true(all(unlist(mod_1$model_df$extras)))
  expect_true(is.null(mod_1$imp))

  check_folds <- function(x, ...) {
    rlang::eval_tidy(x$call$pmethod) == "backward" &
      rlang::eval_tidy(x$call$nfold) == "5" &
      is.null(x$glm.coefficients)
  }
  mod_2 <-
    bagger(
      mpg ~ .,
      data = mtcars,
      model = "MARS",
      nfold = 5,
      pmethod = "backward",
      control = bag_control(var_imp = TRUE),
      extract = check_folds
    )
  expect_true(all(unlist(mod_2$model_df$extras)))
  expect_true(inherits(mod_2$imp, "tbl_df"))

  check_classif <- function(x, ...) {
    !is.null(x$glm.coefficients)
  }
  expect_warning(
    mod_3 <-
      bagger(
        Class ~ .,
        data = two_class_dat,
        model = "MARS",
        control = bag_control(var_imp = TRUE),
        extract = check_classif
      ),
    "fitted probabilities numerically 0"
  )
  expect_true(all(unlist(mod_3$model_df$extras)))
  expect_true(inherits(mod_3$imp, "tbl_df"))
})
