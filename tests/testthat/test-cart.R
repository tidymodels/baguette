library(testthat)
library(purrr)
library(yardstick)

context("CART models")

# ------------------------------------------------------------------------------

data("two_class_dat", package = "modeldata")

# ------------------------------------------------------------------------------

num_leaves <- function(x, ...) {
  sum(x$frame$var == "<leaf>")
}

get_method <- function(x, ...) {
  x$method
}

get_loss <- function(x, ...) {
  x$parms$loss
}

data("two_class_dat", package = "modeldata")

# ------------------------------------------------------------------------------

test_that('check CART opt', {

  mod_1 <-
    bagger(
      Sepal.Width ~ .,
      data = iris,
      base_model = "CART",
      opt = list(method = "anova"),
      extract = get_method
    )
  mod_2 <-
    bagger(
      Sepal.Width ~ .,
      data = iris,
      base_model = "CART",
      opt = list(maxdepth = 1),
      extract = num_leaves
    )
  lmat <- matrix(c(0, 1, 2, 0), byrow = TRUE, nrow = 2)
  mod_3 <-
    bagger(
      Class ~ .,
      data = two_class_dat,
      base_model = "CART",
      opt = list(parms = list(loss = lmat)),
      control = control_bag(var_imp = TRUE),
      extract = get_loss
    )

  expect_true(all(unlist(mod_1$model_df$extras) == "anova"))
  expect_true(all(unlist(mod_2$model_df$extras) == 2))
  expect_true(all(map_lgl(mod_3$model_df$extras, ~ is.matrix(.x))))
  expect_true(inherits(mod_3$imp, "tbl_df"))
  expect_true(isTRUE(all(sort(mod_3$imp$term) == LETTERS[1:2])))
})

# ------------------------------------------------------------------------------

test_that('check model reduction', {
  set.seed(36323)
  reduced <-
    bagger(
      Species ~ .,
      data = iris,
      base_model = "CART",
      times = 3
    )
  expect_true(length(reduced$model_df$model[[1]]$fit$y) == 0)
  expect_true(length(reduced$model_df$model[[1]]$fit$control) == 2)
  expect_equal(reduced$model_df$model[[1]]$fit$call, rlang::call2("dummy_call"))
  expect_identical(attr(reduced$model_df$model[[1]]$fit$terms, ".Environment"), rlang::base_env())

  set.seed(36323)
  full <-
    bagger(
      Species ~ .,
      data = iris,
      base_model = "CART",
      times = 3,
      control = control_bag(reduce = FALSE)
    )

  expect_true(length(full$model_df$model[[1]]$fit$y) > 0)
  expect_true(length(full$model_df$model[[1]]$fit$control) > 1)
  expect_true(is.call(full$model_df$model[[1]]$fit$call))
  expect_false(
    isTRUE(
      all.equal(attr(full$model_df$model[[1]]$fit$terms, ".Environment"),
                rlang::base_env()
      )
    )
  )

})


# ------------------------------------------------------------------------------

test_that('check CART parsnip interface', {
  set.seed(4779)
  expect_error(
    reg_mod <- bag_tree(cost_complexity = .001, min_n = 3) %>%
      set_engine("rpart", times = 3) %>%
      set_mode("regression") %>%
      fit(mpg ~ ., data = mtcars),
    regexp = NA
  )
  expect_true(
    all(purrr::map_lgl(reg_mod$fit$model_df$model, ~ inherits(.x, "model_fit")))
  )
  expect_true(
    all(purrr::map_lgl(reg_mod$fit$model_df$model, ~ inherits(.x$fit, "rpart")))
  )
  expect_error(
    reg_mod_pred <- predict(reg_mod, mtcars[1:5, -1]),
    regexp = NA
  )
  expect_true(tibble::is_tibble(reg_mod_pred))
  expect_equal(nrow(reg_mod_pred), 5)
  expect_equal(names(reg_mod_pred), ".pred")

  set.seed(4779)
  expect_error(
    class_cost <- bag_tree(min_n = 3, class_cost = 2) %>%
      set_engine("rpart", times = 3) %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat),
    regexp = NA
  )
  expect_true(
    all(purrr::map_lgl(class_cost$fit$model_df$model, ~ inherits(.x, "model_fit")))
  )
  expect_true(
    all(purrr::map_lgl(class_cost$fit$model_df$model, ~ inherits(.x$fit, "rpart")))
  )
  expect_error(
    class_cost_pred <- predict(class_cost, two_class_dat[1:5, -3]),
    regexp = NA
  )
  expect_true(tibble::is_tibble(class_cost_pred))
  expect_equal(nrow(class_cost_pred), 5)
  expect_equal(names(class_cost_pred), ".pred_class")

  expect_error(
    class_cost_prob <- predict(class_cost, two_class_dat[1:5, -3], type = "prob"),
    regexp = NA
  )
  expect_true(tibble::is_tibble(class_cost_prob))
  expect_equal(nrow(class_cost_prob), 5)
  expect_equal(names(class_cost_prob), c(".pred_Class1", ".pred_Class2"))

  # ----------------------------------------------------------------------------

  set.seed(4779)
  expect_error(
    class_mod <- bag_tree(cost_complexity = .001, min_n = 3) %>%
      set_engine("rpart", times = 3) %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat),
    regexp = NA
  )
  expect_true(
    all(purrr::map_lgl(class_mod$fit$model_df$model, ~ inherits(.x, "model_fit")))
  )
  expect_true(
    all(purrr::map_lgl(class_mod$fit$model_df$model, ~ inherits(.x$fit, "rpart")))
  )
  expect_error(
    class_mod_pred <- predict(class_mod, two_class_dat[1:5, -3]),
    regexp = NA
  )
  expect_true(tibble::is_tibble(class_mod_pred))
  expect_equal(nrow(class_mod_pred), 5)
  expect_equal(names(class_mod_pred), ".pred_class")

  expect_error(
    class_mod_prob <- predict(class_mod, two_class_dat[1:5, -3], type = "prob"),
    regexp = NA
  )
  expect_true(tibble::is_tibble(class_mod_prob))
  expect_equal(nrow(class_mod_prob), 5)
  expect_equal(names(class_mod_prob), c(".pred_Class1", ".pred_Class2"))
})

