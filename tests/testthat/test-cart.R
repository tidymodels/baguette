library(testthat)
library(purrr)
library(yardstick)

context("CART models")

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

data("two_class_dat", package = "modeldata")

# ------------------------------------------------------------------------------

test_that('check CART opt', {

  mod_1 <-
    bagger(
      Sepal.Width ~ .,
      data = iris,
      base_model = "CART",
      opt = list(method = "anova"),
      extract = get_method
    )
  mod_2 <-
    bagger(
      Sepal.Width ~ .,
      data = iris,
      base_model = "CART",
      opt = list(maxdepth = 1),
      extract = num_leaves
    )
  lmat <- matrix(c(0, 1, 2, 0), byrow = TRUE, nrow = 2)
  mod_3 <-
    bagger(
      Class ~ .,
      data = two_class_dat,
      base_model = "CART",
      opt = list(parms = list(loss = lmat)),
      control = control_bag(var_imp = TRUE),
      extract = get_loss
    )

  expect_true(all(unlist(mod_1$model_df$extras) == "anova"))
  expect_true(all(unlist(mod_2$model_df$extras) == 2))
  expect_true(all(map_lgl(mod_3$model_df$extras, ~ is.matrix(.x))))
  expect_true(inherits(mod_3$imp, "tbl_df"))
  expect_true(isTRUE(all(sort(mod_3$imp$term) == LETTERS[1:2])))
})

# ------------------------------------------------------------------------------

test_that('check model reduction', {
  set.seed(36323)
  reduced <-
    bagger(
      Species ~ .,
      data = iris,
      base_model = "CART",
      times = 3
    )
  expect_true(length(reduced$model_df$model[[1]]$fit$y) == 0)
  expect_true(length(reduced$model_df$model[[1]]$fit$control) == 2)
  expect_equal(reduced$model_df$model[[1]]$fit$call, rlang::call2("dummy_call"))
  expect_identical(attr(reduced$model_df$model[[1]]$fit$terms, ".Environment"), rlang::base_env())

  set.seed(36323)
  full <-
    bagger(
      Species ~ .,
      data = iris,
      base_model = "CART",
      times = 3,
      control = control_bag(reduce = FALSE)
    )

  expect_true(length(full$model_df$model[[1]]$fit$y) > 0)
  expect_true(length(full$model_df$model[[1]]$fit$control) > 1)
  expect_true(is.call(full$model_df$model[[1]]$fit$call))
  expect_false(
    isTRUE(
      all.equal(attr(full$model_df$model[[1]]$fit$terms, ".Environment"),
                rlang::base_env()
      )
    )
  )

})
