test_that('good values', {
  expect_no_error(
    baguette:::validate_args(
      model = "MARS",
      times = 5L,
      control = control_bag(),
      cost = NULL
    )
  )
})

test_that('bad values', {
  expect_snapshot(
    baguette:::validate_args(
      model = "mars",
      times = 5L,
      control = control_bag(),
      cost = NULL),
    error = TRUE
  )

  expect_snapshot(
    baguette:::validate_args(
      model = "MARS",
      times = 1,
      control = control_bag(),
      cost = NULL
    ),
    error = TRUE
  )

  expect_snapshot(
    baguette:::validate_args(
      model = "MARS",
      times = -1L,
      control = control_bag(),
      cost = NULL
    ),
    error = TRUE
  )

  expect_snapshot(
    baguette:::validate_args(
      model = "MARS",
      times = 5L,
      control = 2,
      cost = NULL
    ),
    error = TRUE
  )

  expect_snapshot(
    bagger(
      Sepal.Length ~ .,
      data = iris,
      times = 2L,
      base_model = "CART",
      cost = 2
    ),
    error = TRUE
  )

})

# ------------------------------------------------------------------------------

test_that('wrong y for C5', {
  skip_if_not_installed("hardhat", minimum_version = "1.4.0.9002")
  expect_snapshot(
    bagger(Sepal.Length ~ ., data = iris, times = 2L, base_model = "C5.0"),
    error = TRUE
  )
})

# ------------------------------------------------------------------------------

test_that('validate imps', {

  expect_snapshot(
    baguette:::validate_importance(
      tibble::tibble(
        terms = letters[1:2],
        value = 1:2,
        std.error = 1:2
      )
    ),
    error = TRUE
  )

  expect_snapshot(
    baguette:::validate_importance(
      data.frame(term = letters[1:2], value = 1:2, std.error = 1:2)
    ),
    error = TRUE
  )
})

# ------------------------------------------------------------------------------

test_that('bad inputs', {
  skip_if_not_installed("earth")
  skip_if_not_installed("modeldata")
  skip_if_not_installed("hardhat", minimum_version = "1.4.0.9002")

  expect_snapshot(
    bagger(mpg ~ ., data = mtcars, base_model = letters[1:2]),
    error = TRUE
  )

  expect_snapshot(
    bagger(mpg ~ ., data = mtcars, base_model = "MARS", cost = 2),
    error = TRUE
  )

  expect_snapshot(
    bagger(mpg ~ ., data = mtcars, base_model = "CART", cost = -2),
    error = TRUE
  )

  expect_snapshot(
    bagger(
      mpg ~ .,
      data = mtcars,
      base_model = "CART",
      cost = matrix(1, ncol = 2, nrow = 1)
    ),
    error = TRUE
  )

  expect_snapshot(
    bagger(
      mpg ~ .,
      data = mtcars,
      base_model = "MARS",
      control = control_bag(extract = 2)
    ),
    error = TRUE
  )

  expect_snapshot(
    bagger(mpg ~ ., data = mtcars, base_model = "C5.0"),
    error = TRUE
  )

  expect_snapshot(
    bagger(wt + mpg ~ ., data = mtcars, base_model = "MARS"),
    error = TRUE
  )

  expect_snapshot(
    predict(bagger(mpg ~ ., data = mtcars, base_model = "MARS"),
            mtcars[1:2, -1],
            type = "potato"),
    error = TRUE
  )

  if (compareVersion(as.character(getRversion()), "3.6.0") > 0) {
    expect_warning(RNGkind(sample.kind = "Rounding"))
  }

  expect_snapshot({
    set.seed(3983)
    predict(bagger(Class ~ ., data = two_class_dat, base_model = "MARS"),
            two_class_dat[1:2, -3],
            type = "topepo")
  },
  error = TRUE
  )

})

# ------------------------------------------------------------------------------

test_that('model failures inputs', {
  bad_iris <- iris
  bad_iris$a <- factor("a", levels = letters[1:2])

  if (compareVersion(as.character(getRversion()), "3.6.0") > 0) {
    expect_warning(RNGkind(sample.kind = "Rounding"))
  }

  expect_snapshot({
    set.seed(459394)
    bagger(a ~ ., data = bad_iris, base_model = "CART", times = 3)
  },
  error = TRUE
  )
})

# ------------------------------------------------------------------------------

test_that('control inputs', {

  expect_snapshot(
    control_bag(reduce = 1:2),
    error = TRUE
  )

  expect_snapshot(
    control_bag(reduce = 1),
    error = TRUE
  )

  expect_snapshot(
    control_bag(sampling = rep("none", 2)),
    error = TRUE
  )

  expect_snapshot(
    control_bag(sampling = 1),
    error = TRUE
  )

  expect_snapshot(
    control_bag(allow_parallel = 1:2),
    error = TRUE
  )

  expect_snapshot(
    control_bag(allow_parallel = 1),
    error = TRUE
  )

  expect_snapshot(
    control_bag(var_imp = 1:2),
    error = TRUE
  )

  expect_snapshot(
    control_bag(var_imp = 1),
    error = TRUE
  )

})
