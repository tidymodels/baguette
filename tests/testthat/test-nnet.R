library(testthat)
library(purrr)
library(yardstick)

context("nnet models")

# ------------------------------------------------------------------------------

data("two_class_dat", package = "modeldata")

# ------------------------------------------------------------------------------

get_censored <- function(x, ...) {
  x$censored
}

# ------------------------------------------------------------------------------

test_that('check nnet opt', {
  expect_error(
    mod_1 <-
      bagger(
        Class ~ .,
        data = two_class_dat,
        base_model = "nnet",
        control = control_bag(extract = get_censored),
        hidden_units = 3
      ),
    regex = NA
  )
  expect_equal(mod_1$model_df$model[[1]]$fit$n[2], 3)
})

# ------------------------------------------------------------------------------

test_that('check model reduction', {
  set.seed(36323)
  reduced <-
    bagger(
      mpg ~ .,
      data = mtcars,
      base_model = "nnet",
      times = 3
    )
  expect_true(!any(names(reduced$model_df$model[[1]]$fit) == "residuals"))
  expect_true(!any(names(reduced$model_df$model[[1]]$fit) == "fitted.values"))
  expect_equal(reduced$model_df$model[[1]]$fit$call, rlang::call2("dummy_call"))
  expect_identical(attr(reduced$model_df$model[[1]]$fit$terms, ".Environment"), rlang::base_env())

  set.seed(36323)
  full <-
    bagger(
      mpg ~ .,
      data = mtcars,
      base_model = "nnet",
      times = 3,
      control = control_bag(reduce = FALSE)
    )

  expect_true(any(names(full$model_df$model[[1]]$fit) == "residuals"))
  expect_true(any(names(full$model_df$model[[1]]$fit) == "fitted.values"))
  expect_true(is.call(full$model_df$model[[1]]$fit$call))
  expect_false(
    isTRUE(
      all.equal(attr(full$model_df$model[[1]]$fit$terms, ".Environment"),
                rlang::base_env()
      )
    )
  )

})

