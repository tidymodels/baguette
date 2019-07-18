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

data("two_class_dat", package = "rsample")

# ------------------------------------------------------------------------------

test_that('check CART opt', {

  mod_1 <-
    bagger(
      Sepal.Width ~ .,
      data = iris,
      model = "CART",
      opt = list(method = "anova"),
      extract = get_method
    )
  mod_2 <-
    bagger(
      Sepal.Width ~ .,
      data = iris,
      model = "CART",
      opt = list(maxdepth = 1),
      extract = num_leaves
    )
  lmat <- matrix(c(0, 1, 2, 0), byrow = TRUE, nrow = 2)
  mod_3 <-
    bagger(
      Class ~ .,
      data = two_class_dat,
      model = "CART",
      opt = list(parms = list(loss = lmat)),
      control = bag_control(var_imp = TRUE),
      extract = get_loss
    )

  expect_true(all(unlist(mod_1$model_df$extras) == "anova"))
  expect_true(all(unlist(mod_2$model_df$extras) == 2))
  expect_true(all(map_lgl(mod_3$model_df$extras, ~ is.matrix(.x))))
  expect_true(inherits(mod_3$imp, "tbl_df"))
  expect_true(isTRUE(all(sort(mod_3$imp$term) == LETTERS[1:2])))
})

# ------------------------------------------------------------------------------

test_that('check CART OOB', {
  ms_1 <- metric_set(rsq)
  mod_1 <-
    bagger(
      Sepal.Width ~ .,
      data = iris,
      model = "CART",
      control = bag_control(oob = ms_1)
    )
  expect_true(all(mod_1$oob$.metric == "rsq"))
  expect_true(all(!is.na(mod_1$oob$mean)))

  ms_2 <- metric_set(accuracy, roc_auc)
  mod_2 <-
    bagger(
      Class ~ .,
      data = two_class_dat,
      model = "CART",
      control = bag_control( oob = ms_2)
    )
  expect_true(sum(mod_2$oob$.metric == "accuracy") == 1)
  expect_true(sum(mod_2$oob$.metric == "roc_auc") == 1)
  expect_true(all(!is.na(mod_2$oob$mean)))

  mod_3 <-
    bagger(
      Sepal.Width ~ .,
      data = iris,
      model = "CART",
      control = bag_control(oob = ms_2)
    )
  expect_true(all(mod_3$oob$.metric == "failed"))
  expect_true(all(is.na(mod_3$oob$mean)))

  mod_4 <-
    bagger(
      Species ~ .,
      data = iris,
      model = "CART",
      control = bag_control(oob = ms_2)
    )
  expect_true(sum(mod_4$oob$.metric == "accuracy") == 1)
  expect_true(sum(mod_4$oob$.metric == "roc_auc") == 1)
  expect_true(all(!is.na(mod_4$oob$mean)))
})
