library(testthat)
library(recipes)

context("model interfaces")

# ------------------------------------------------------------------------------

test_that('recipe execution', {
  # check to make sure that prepped data are given to model
  rec <-
    recipe(Sepal.Length ~ ., data = iris) %>%
    step_log(starts_with("Petal")) %>%
    step_sqrt(Sepal.Length, skip = TRUE)

  check_ranges <- function(x, ...) {
    max(x$model$Petal.Width) <= log(max(iris$Petal.Width)) &
      max(x$model$Petal.Length) <= log(max(iris$Petal.Length)) &
      max(x$model$.outcome) <= sqrt(max(iris$Sepal.Length))
  }

  expect_error(
    mod <-
      bagger(
        rec,
        data = iris,
        model = "CART",
        opt = list(model = TRUE),
        extract = check_ranges
      ),
    regexp = NA)

  expect_true(all(unlist(mod$model_df$extras)))
})


test_that('formula execution', {
  # check to make sure that appropriate data are given to model
  check_columns <- function(x, ...) {
    max(x$model$`log(Sepal.Width)`) <= log(max(iris$Sepal.Width)) &
    is.factor(x$model$Species)
  }

  expect_error(
    mod <-
      bagger(
        Sepal.Length ~ log(Sepal.Width) + Species,
        data = iris,
        model = "CART",
        opt = list(model = TRUE),
        extract = check_columns
      ),
    regex = NA)

  expect_true(all(unlist(mod$model_df$extras)))
})
