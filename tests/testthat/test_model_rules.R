library(testthat)
# ------------------------------------------------------------------------------

test_that('check cubist opt', {

  check_unbiased <- function(x, ...) {
    x$control$unbiased
  }
  mod_1 <-
    bagger(
      mpg ~ .,
      data = mtcars,
      model = "model_rules",
      opt = list(unbiased = TRUE),
      extract = check_unbiased
    )
  expect_true(all(unlist(mod_1$model_df$extras)))
  expect_true(is.null(mod_1$imp))

  mod_2 <-
    bagger(
      mpg ~ .,
      data = mtcars,
      model = "model_rules",
      var_imp = TRUE
    )
  expect_true(inherits(mod_2$imp, "tbl_df"))
})

# ------------------------------------------------------------------------------

test_that('check cubist OOB', {
  ms_1 <- metric_set(rsq)
  mod_1 <-
    bagger(
      Sepal.Width ~ .,
      data = iris,
      model = "model_rules",
      oob = ms_1
    )
  expect_true(all(mod_1$oob$.metric == "rsq"))
  expect_true(all(!is.na(mod_1$oob$mean)))

})

