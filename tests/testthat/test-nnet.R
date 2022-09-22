
test_that('check nnet parsnip interface', {
  data(two_class_dat, package = "modeldata")

  set.seed(4779)
  expect_error(
    reg_mod <- bag_mlp() %>%
      set_engine("nnet", times = 3) %>%
      set_mode("regression") %>%
      fit(mpg ~ ., data = mtcars),
    regexp = NA
  )
  expect_true(
    all(purrr::map_lgl(reg_mod$fit$model_df$model, ~ inherits(.x, "model_fit")))
  )
  expect_true(
    all(purrr::map_lgl(reg_mod$fit$model_df$model, ~ inherits(.x$fit, "nnet")))
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
    class_cost <- bag_mlp() %>%
      set_engine("nnet", times = 3) %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat),
    regexp = NA
  )
  expect_true(
    all(purrr::map_lgl(class_cost$fit$model_df$model, ~ inherits(.x, "model_fit")))
  )
  expect_true(
    all(purrr::map_lgl(class_cost$fit$model_df$model, ~ inherits(.x$fit, "nnet")))
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

})

test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("bag_mlp", "_pkgs")) %>%
      dplyr::filter(engine == "nnet", mode == "classification") %>%
      dplyr::pull(pkg),
    list(c("nnet", "baguette"))
  )

  expect_identical(
    get_from_env(paste0("bag_mlp", "_pkgs")) %>%
      dplyr::filter(engine == "nnet", mode == "regression") %>%
      dplyr::pull(pkg),
    list(c("nnet", "baguette"))
  )
})

