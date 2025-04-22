test_that('check CART opt', {
  skip_if_not_installed("modeldata")

  mod_1 <-
    bagger(
      Sepal.Width ~ .,
      data = iris,
      base_model = "CART",
      control = control_bag(extract = get_method),
      method = "anova"
    )
  mod_2 <-
    bagger(
      Sepal.Width ~ .,
      data = iris,
      base_model = "CART",
      control = control_bag(extract = num_leaves),
      maxdepth = 1
    )
  lmat <- matrix(c(0, 1, 2, 0), byrow = TRUE, nrow = 2)
  mod_3 <-
    bagger(
      Class ~ .,
      data = two_class_dat,
      base_model = "CART",
      control = control_bag(var_imp = TRUE, extract = get_loss),
      parms = list(loss = lmat)
    )

  expect_true(all(unlist(mod_1$model_df$extras) == "anova"))
  expect_true(all(unlist(mod_2$model_df$extras) == 2))
  expect_true(all(map_lgl(mod_3$model_df$extras, ~ is.matrix(.x))))
  expect_true(inherits(mod_3$imp, "tbl_df"))
  expect_true(isTRUE(all(sort(mod_3$imp$term) == LETTERS[1:2])))

  # Check for models with no importances
  rm_imp <- function(x) {
    x$fit$variable.importance <- NULL
    x
  }
  one_missing <- mod_1$model_df
  one_missing$model[[1]] <- rm_imp(one_missing$model[[1]])
  expect_error(
    one_missing_stats <-
      baguette:::compute_imp(one_missing, baguette:::cart_imp, compute = TRUE),
    regex = NA
  )
  expect_true(all(one_missing_stats$used <= 10))

  all_missing <- mod_1$model_df
  all_missing$model <- purrr::map(one_missing$model, rm_imp)
  expect_error(
    all_missing_stats <-
      baguette:::compute_imp(all_missing, baguette:::cart_imp, compute = TRUE),
    regex = NA
  )
  expect_true(nrow(all_missing_stats) == 0)
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
  skip_if_not_installed("modeldata")

  set.seed(4779)
  expect_error(
    reg_mod <- bag_tree(cost_complexity = .001, min_n = 3) |>
      set_engine("rpart", times = 3) |>
      set_mode("regression") |>
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
    class_cost <- bag_tree(min_n = 3, class_cost = 2) |>
      set_engine("rpart", times = 3) |>
      set_mode("classification") |>
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
    class_mod <- bag_tree(cost_complexity = .001, min_n = 3) |>
      set_engine("rpart", times = 3) |>
      set_mode("classification") |>
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

test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("bag_tree", "_pkgs")) |>
      dplyr::filter(engine == "rpart", mode == "classification") |>
      dplyr::pull(pkg),
    list(c("rpart", "baguette"))
  )

  expect_identical(
    get_from_env(paste0("bag_tree", "_pkgs")) |>
      dplyr::filter(engine == "rpart", mode == "regression") |>
      dplyr::pull(pkg),
    list(c("rpart", "baguette"))
  )
})



test_that('case weights', {
  skip_if_not_installed("modeldata")

  data("two_class_dat", package = "modeldata")
  set.seed(1)
  wts <- runif(nrow(two_class_dat))
  wts <- ifelse(wts < 1/5, 0, 1)

  expect_error({
    set.seed(1)
    wts_fit <- bagger(Class ~ A + B, data = two_class_dat, weights = wts)
  },
  regexp = NA
  )

  set.seed(1)
  fit <- bagger(Class ~ A + B, data = two_class_dat)
  expect_true(!identical(wts_fit$imp, fit$imp))
})

