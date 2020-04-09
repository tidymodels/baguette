library(testthat)

context("validation code")

# ------------------------------------------------------------------------------

data("two_class_dat", package = "modeldata")

# ------------------------------------------------------------------------------

test_that('good values', {
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      times = 5L,
      control = control_bag(),
      cost = NULL
    ),
    regexp = NA
  )
})

test_that('bad values', {
  expect_error(
    baguette:::validate_args(
      model = "mars",
      times = 5L,
      control = control_bag(),
      cost = NULL
    ),
    regexp = "`base_model`",
    class = "rlang_error"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      times = 1,
      control = control_bag(),
      cost = NULL
    ),
    regexp = "integer"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      times = -1L,
      control = control_bag(),
      cost = NULL
    ),
    regexp = "integer"
  )
  expect_error(
    baguette:::validate_args(
      model = "MARS",
      times = 5L,
      control = 2,
      cost = NULL
    ),
    regexp = "should be a list"
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

# ------------------------------------------------------------------------------

test_that('bad inputs', {
  expect_error(
    bagger(mpg ~ ., data = mtcars, base_model = letters[1:2]),
    "should be a single character value."
  )
  expect_error(
    bagger(mpg ~ ., data = mtcars, base_model = "MARS", cost = 2),
    "should be either 'CART' or 'C5.0'"
  )
  expect_error(
    bagger(mpg ~ ., data = mtcars, base_model = "CART", cost = -2),
    "`cost` should be non-negative"
  )
  expect_error(
    bagger(mpg ~ ., data = mtcars, base_model = "CART", cost = matrix(-2, ncol = 2, nrow = 2)),
    "`cost` should be non-negative"
  )
  expect_error(
    bagger(mpg ~ ., data = mtcars, base_model = "MARS", control = control_bag(extract = 2)),
    "`extract` should be NULL or a function"
  )
  expect_error(
    bagger(mpg ~ ., data = mtcars, base_model = "C5.0"),
    "All outcomes must be factors, but the following are not"
  )
  expect_error(
    bagger(wt + mpg ~ ., data = mtcars, base_model = "MARS"),
    "The outcome must be univariate"
  )
  expect_error(
    predict(bagger(mpg ~ ., data = mtcars, base_model = "MARS"),
            mtcars[1:2, -1],
            type = "potato"),
    "`type` should be 'numeric'"
  )
  if (compareVersion(as.character(getRversion()), "3.6.0") > 0) {
    expect_warning(RNGkind(sample.kind = "Rounding"))
  }
  set.seed(3983)
  expect_warning(
    expect_error(
      predict(bagger(Class ~ ., data = two_class_dat, base_model = "MARS"),
              two_class_dat[1:2, -3],
              type = "topepo"),
      "`type` should be either 'class' or 'prob'"
    ),
    "fitted probabilities numerically 0 or 1 occurred"
  )
})

# ------------------------------------------------------------------------------

test_that('model failures inputs', {
  bad_iris <- iris
  bad_iris$a <- factor("a", levels = letters[1:2])

  if (compareVersion(as.character(getRversion()), "3.6.0") > 0) {
    expect_warning(RNGkind(sample.kind = "Rounding"))
  }
  set.seed(459394)
  expect_error(
    bagger(a ~ ., data = bad_iris, base_model = "CART", times = 3),
    "All of the models failed"
  )

})


# ------------------------------------------------------------------------------

test_that('control inputs', {
  expect_error(
    control_bag(var_imp = 1),
    "`var_imp` should be a single logical value."
  )
  expect_error(
    control_bag(var_imp = 1:2),
    "`var_imp` should be a single logical value."
  )
  expect_error(
    control_bag(allow_parallel = 1),
    "`allow_parallel` should be a single logical value."
  )
  expect_error(
    control_bag(allow_parallel = 1:2),
    "`allow_parallel` should be a single logical value."
  )
  expect_error(
    control_bag(sampling = 1),
    "`sampling` should be either 'none' or 'down'"
  )
  expect_error(
    control_bag(sampling = rep("none", 2)),
    "`sampling` should be either 'none' or 'down'"
  )
  expect_error(
    control_bag(reduce = 1),
    "`reduce` should be a single logical value."
  )
  expect_error(
    control_bag(reduce = 1:2),
    "`reduce` should be a single logical value."
  )
})
