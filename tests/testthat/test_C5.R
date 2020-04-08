library(testthat)

context("C5.0 models")

# ------------------------------------------------------------------------------

test_that('check C5.0 opt', {
  check_rules <- function(x, ...) {
    x$tree == "" & nchar(x$rules) > 10
  }
  mod_1 <-
    bagger(
      Species ~ .,
      data = iris,
      base_model = "C5.0",
      opt = list(rules = TRUE),
      extract = check_rules
    )
  expect_true(all(unlist(mod_1$model_df$extras)))
  expect_true(!is.null(mod_1$imp))

  check_winnow <- function(x, ...) {
    x$tree == "" &
      nchar(x$rules) > 10 &
      x$control$bands == 3
  }
  mod_2 <-
    bagger(
      Species ~ .,
      data = iris,
      base_model = "C5.0",
      opt = list(rules = TRUE, bands = 3),
      control = control_bag(var_imp = TRUE),
      extract = check_winnow
    )
  expect_true(all(unlist(mod_2$model_df$extras)))
  expect_true(inherits(mod_2$imp, "tbl_df"))
})
