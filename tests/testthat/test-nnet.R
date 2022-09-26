
test_that('check nnet parsnip interface', {
  skip_if_not_installed("nnet")
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

test_that('variable importance', {
  skip_if_not_installed("nnet")

  # See inst/helper-objects-for-testing.R
  # Values from another implementation
  exp_vip <-
    tibble::tribble(
 ~predictor,      ~importance,
      "cyl", 10.4382541964352,
      "disp", 6.40411349529868,
      "hp", 10.5671716879969,
      "drat",  11.816366389055,
      "wt", 9.78795915963821,
      "qsec", 18.3934915487232,
      "vs", 6.28589044459608,
      "am", 5.70032668136104,
      "gear", 12.9721190483202,
      "carb",  7.6343073485756
    )

  set.seed(1)
  reg_mod <- nnet::nnet(mpg ~ ., data = mtcars, size = 3, trace = FALSE)
  baguette_imp <- baguette:::nnet_imp_garson(reg_mod)
  expect_equal(exp_vip, baguette_imp, tolerance = 0.0001)

})

