library(testthat)

context("validation code")

# ------------------------------------------------------------------------------

test_that('good values', {
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      B = 5L,
      opt = list(x = 1),
      var_imp = TRUE,
      oob = NULL,
      extract = NULL
    ),
    regexp = NA
  )
})

test_that('bad values', {
  expect_error(
    baguette:::validate_args(
      model = "mars",
      B = 5L,
      opt = list(x = 1),
      var_imp = TRUE,
      oob = NULL,
      extract = NULL
    ),
    regexp = "`model`"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      B = 1,
      opt = list(x = 1),
      var_imp = TRUE,
      oob = NULL,
      extract = NULL
    ),
    regexp = "integer"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      B = -1L,
      opt = list(x = 1),
      var_imp = TRUE,
      oob = NULL,
      extract = NULL
    ),
    regexp = "integer"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      B = 2L,
      opt = 2,
      var_imp = TRUE,
      oob = NULL,
      extract = NULL
    ),
    regexp = "should be NULL or a named list"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      B = 5L,
      opt = list(x = 1),
      var_imp = 5,
      oob = NULL,
      extract = NULL
    ),
    regexp = "should be single logical"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      B = 5L,
      opt = list(x = 1),
      var_imp = TRUE,
      oob = NULL,
      extract = function(x, y) 2
    ),
    regexp = "2nd"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      B = 5L,
      opt = list(x = 1),
      var_imp = TRUE,
      oob = NULL,
      extract = function(x) 2
    ),
    regexp = "two arguments"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      B = 5L,
      opt = list(x = 1),
      var_imp = TRUE,
      oob = TRUE,
      extract = NULL
    ),
    regexp = "metric_set"
  )
})

# ------------------------------------------------------------------------------

test_that('wrong y for cubist', {
  expect_error(
    bagger(Species ~ ., data = iris, B = 2L, model = "model_rules"),
    regexp = "must be numeric"
  )
})

test_that('wrong y for C5', {
  expect_error(
    bagger(Sepal.Length ~ ., data = iris, B = 2L, model = "C5.0"),
    regexp = "must be a factor"
  )
})

# ------------------------------------------------------------------------------

test_that('catastrophic failures', {
  expect_error(
    bagger(Sepal.Length ~ ., data = iris, B = 2L, model = "CART", opt = list(cost = 2)),
    regexp = "All of the models"
  )
})
