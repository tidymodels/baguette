library(testthat)

context("validation code")

# ------------------------------------------------------------------------------

test_that('good values', {
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      B = 5L,
      opt = list(x = 1),
      control = list(),
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
      control = list(),
      extract = NULL
    ),
    regexp = "`model`"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      B = 1,
      opt = list(x = 1),
      control = list(),
      extract = NULL
    ),
    regexp = "integer"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      B = -1L,
      opt = list(x = 1),
      control = list(),
      extract = NULL
    ),
    regexp = "integer"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      B = 2L,
      opt = 2,
      control = list(),
      extract = NULL
    ),
    regexp = "should be NULL or a named list"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      B = 5L,
      opt = list(x = 1),
      control = 2,
      extract = NULL
    ),
    regexp = "should be a list"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      B = 5L,
      opt = list(x = 1),
      control = list(),
      extract = function(x, y) 2
    ),
    regexp = "2nd"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      B = 5L,
      opt = list(x = 1),
      control = list(),
      extract = function(x) 2
    ),
    regexp = "two arguments"
  )
})

# ------------------------------------------------------------------------------

test_that('wrong y for cubist', {
  expect_error(
    bagger(Species ~ ., data = iris, B = 2L, model = "model_rules"),
    regexp = "cubist models require a numeric outcome"
  )
})

test_that('wrong y for C5', {
  expect_error(
    bagger(Sepal.Length ~ ., data = iris, B = 2L, model = "C5.0"),
    regexp = "must be factors"
  )
})

# ------------------------------------------------------------------------------

test_that('catastrophic failures', {
  expect_error(
    bagger(Sepal.Length ~ ., data = iris, B = 2L, model = "CART", opt = list(cost = 2)),
    regexp = "All of the models failed"
  )
})
