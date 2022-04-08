library(recipes)

test_that('recipe execution', {
  # check to make sure that prepped data are given to model
  rec <-
    recipe(Sepal.Length ~ ., data = iris) %>%
    step_log(starts_with("Petal")) %>%
    step_sqrt(Sepal.Length, skip = TRUE)

  check_ranges <- function(x) {
    all(max(x$model$Petal.Width) <= log(max(iris$Petal.Width))) &
      all(max(x$model$Petal.Length) <= log(max(iris$Petal.Length))) &
      all(max(x$model$.outcome) <= sqrt(max(iris$Sepal.Length)))
  }

  expect_error(
    mod <-
      bagger(
        rec,
        data = iris,
        base_model = "CART",
        control = control_bag(reduce = FALSE, extract = check_ranges),
        model = TRUE
      ),
    regexp = NA)

  expect_true(all(unlist(mod$model_df$extras)))
})

# ------------------------------------------------------------------------------

test_that('var_imp', {
  mod <- bagger(mpg ~ ., data = mtcars, base_model = "MARS", times = 2)
  expect_true(tibble::is_tibble(var_imp(mod)))
  expect_equal(names(var_imp(mod)), c("term", "value", "std.error", "used"))
  expect_equal(var_imp(mod), mod$imp)
})
