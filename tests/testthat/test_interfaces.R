library(testthat)
library(recipes)

test_that('recipe execution', {
  # check to make sure that prepped data are given to model
  rec <-
    recipe(Sepal.Length ~ ., data = iris) %>%
    step_log(starts_with("Petal")) %>%
    step_sqrt(Sepal.Length, skip = TRUE)

  check_ranges <- function(x, ...) {
    max(x$fit$model$Petal.Width) <= log(max(iris$Petal.Width)) &
      max(x$fit$model$Petal.Length) <= log(max(iris$Petal.Length)) &
      max(x$fit$model$.outcome) <= sqrt(max(iris$Sepal.Length))
  }

  mod <-
    bagger(
      rec,
      data = iris,
      model = "CART",
      opt = list(model = TRUE),
      extract = check_ranges
    )
  expect_true(all(unlist(mod$model_df$extras)))
})


test_that('formula execution', {
  # check to make sure that appropriate data are given to model
  check_columns <- function(x, ...) {
    max(x$fit$model$`log(Sepal.Width)`) <= log(max(iris$Sepal.Width)) &
    is.factor(x$fit$model$Species)
  }

  mod <-
    bagger(
      Sepal.Length ~ log(Sepal.Width) + Species,
      data = iris,
      model = "CART",
      opt = list(model = TRUE),
      extract = check_columns
    )
  expect_true(all(unlist(mod$model_df$extras)))
})
