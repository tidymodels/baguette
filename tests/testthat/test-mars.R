library(testthat)
library(rlang)

context("MARS models")

# ------------------------------------------------------------------------------

data("two_class_dat", package = "modeldata")

# ------------------------------------------------------------------------------

test_that('check mars opt', {
  set.seed(36323)
  check_pruning <- function(x, ...) {
    rlang::eval_tidy(x$call$pmethod) == "backward"
  }
  mod_1 <-
    bagger(
      mpg ~ .,
      data = mtcars,
      base_model = "MARS",
      opt = list(pmethod = "backward"),
      control = control_bag(var_imp = FALSE),
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
      base_model = "MARS",
      opt = list(nfold = 5, pmethod = "backward"),
      control = control_bag(var_imp = TRUE),
      extract = check_folds
    )
  expect_true(all(unlist(mod_2$model_df$extras)))
  expect_true(inherits(mod_2$imp, "tbl_df"))

  check_classif <- function(x, ...) {
    !is.null(x$glm.coefficients)
  }

  # For correct random numbers
  skip_if(compareVersion(as.character(getRversion()), "3.6.0") < 0)
  expect_warning(
    mod_3 <-
      bagger(
        Class ~ .,
        data = two_class_dat,
        base_model = "MARS",
        control = control_bag(var_imp = TRUE),
        extract = check_classif
      ),
    "fitted probabilities numerically 0"
  )
  expect_true(all(unlist(mod_3$model_df$extras)))
  expect_true(inherits(mod_3$imp, "tbl_df"))
})

# ------------------------------------------------------------------------------

test_that('check model reduction', {
  set.seed(36323)
  reduced <-
    bagger(
      mpg ~ .,
      data = mtcars,
      base_model = "MARS",
      times = 3
    )
  expect_false(is.matrix(reduced$model_df$model[[1]]$fit$y))
  expect_equal(reduced$model_df$model[[1]]$fit$call, rlang::call2("dummy_call"))
  expect_equal(reduced$model_df$model[[1]]$fit$residuals, numeric(0))

  set.seed(36323)
  full <-
    bagger(
      mpg ~ .,
      data = mtcars,
      base_model = "MARS",
      times = 3,
      control = control_bag(reduce = FALSE)
    )

  expect_true(is.matrix(full$model_df$model[[1]]$fit$y))
  expect_true(is.call(full$model_df$model[[1]]$fit$call))
  expect_true(is.matrix(full$model_df$model[[1]]$fit$residuals))

})

