library(testthat)

context("validation code")

# ------------------------------------------------------------------------------

test_that('good values', {
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      opt = NULL,
      times = 5L,
      control = control_bag(),
      extract = NULL,
      cost = NULL
    ),
    regexp = NA
  )
})

test_that('bad values', {
  expect_error(
    baguette:::validate_args(
      model = "mars",
      opt = NULL,
      times = 5L,
      control = control_bag(),
      extract = NULL,
      cost = NULL
    ),
    regexp = "`base_model`",
    class = "rlang_error"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      opt = NULL,
      times = 1,
      control = control_bag(),
      extract = NULL,
      cost = NULL
    ),
    regexp = "integer"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      opt = NULL,
      times = -1L,
      control = control_bag(),
      extract = NULL,
      cost = NULL
    ),
    regexp = "integer"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      opt = NULL,
      times = 5L,
      control = 2,
      extract = NULL,
      cost = NULL
    ),
    regexp = "should be a list"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      opt = NULL,
      times = 5L,
      control = control_bag(),
      extract = function(x, y) 2,
      cost = NULL
    ),
    regexp = "2nd"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      opt = NULL,
      times = 5L,
      control = control_bag(),
      extract = function(x) 2,
      cost = NULL
    ),
    regexp = "two arguments"
  )
  expect_error(
    bagger(Sepal.Length ~ ., data = iris, times = 2L, base_model = "CART", cost = 2),
    regexp = "`cost` can only be a scalar"
  )
})

# ------------------------------------------------------------------------------

test_that('wrong y for C5', {
  expect_error(
    bagger(Sepal.Length ~ ., data = iris, times = 2L, base_model = "C5.0"),
    regexp = "must be factors"
  )
})

# ------------------------------------------------------------------------------

test_that('validate imps', {
  expect_error(
    baguette:::validate_importance(
      tibble::tibble(
        terms = letters[1:2],
        value = 1:2,
        std.error = 1:2
      )
    )
  )
  expect_error(
    baguette:::validate_importance(
      data.frame(term = letters[1:2],
                 value = 1:2,
                 std.error = 1:2)
    )
  )
})
