library(testthat)

context("validation code")

# ------------------------------------------------------------------------------

test_that('good values', {
  expect_error(
    baguette:::validate_args(
      base_model = "MARS",
      times = 5L,
      .control = bag_control(),
      extract = NULL
    ),
    regexp = NA
  )
})

test_that('bad values', {
  expect_error(
    baguette:::validate_args(
      base_model = "mars",
      times = 5L,
      .control = bag_control(),
      extract = NULL
    ),
    regexp = "`base_model`"
  )
  expect_error(
    baguette:::validate_args(
      base_model = "MARS",
      times = 1,
      .control = bag_control(),
      extract = NULL
    ),
    regexp = "integer"
  )
  expect_error(
    baguette:::validate_args(
      base_model = "MARS",
      times = -1L,
      .control = bag_control(),
      extract = NULL
    ),
    regexp = "integer"
  )
  expect_error(
    baguette:::validate_args(
      base_model = "MARS",
      times = 5L,
      .control = 2,
      extract = NULL
    ),
    regexp = "should be a list"
  )
  expect_error(
    baguette:::validate_args(
      base_model = "MARS",
      times = 5L,
      .control = bag_control(),
      extract = function(x, y) 2
    ),
    regexp = "2nd"
  )
  expect_error(
    baguette:::validate_args(
      base_model = "MARS",
      times = 5L,
      .control = bag_control(),
      extract = function(x) 2
    ),
    regexp = "two arguments"
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

test_that('catastrophic failures', {
  expect_error(
    bagger(Sepal.Length ~ ., data = iris, times = 2L, base_model = "CART", cost = 2),
    regexp = "All of the models failed"
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
