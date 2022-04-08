data("two_class_dat", package = "modeldata")

# ------------------------------------------------------------------------------

test_that('check C5.0 opt', {
  check_rules <- function(x, ...) {
    x$tree == "" & nchar(x$rules) > 10
  }
  mod_1 <-
    bagger(
      Species ~ .,
      data = iris,
      base_model = "C5.0",
      control = control_bag(extract = check_rules),
      rules = TRUE
    )
  expect_true(all(unlist(mod_1$model_df$extras)))
  expect_true(!is.null(mod_1$imp))

  check_winnow <- function(x, ...) {
    x$tree == "" &
      nchar(x$rules) > 10 &
      x$control$bands == 3
  }
  mod_2 <-
    bagger(
      Species ~ .,
      data = iris,
      base_model = "C5.0",
      control = control_bag(var_imp = TRUE, extract = check_winnow),
      rules = TRUE,
      bands = 3
    )
  expect_true(all(unlist(mod_2$model_df$extras)))
  expect_true(inherits(mod_2$imp, "tbl_df"))
})


# ------------------------------------------------------------------------------

test_that('check model reduction', {
  set.seed(36323)
  reduced <-
    bagger(
      Species ~ .,
      data = iris,
      base_model = "C5.0",
      times = 3
    )
  expect_true(length(reduced$model_df$model[[1]]$fit$control) == 1)
  expect_equal(reduced$model_df$model[[1]]$fit$call, rlang::call2("dummy_call"))
  expect_equal(reduced$model_df$model[[1]]$fit$output, character(0))

  set.seed(36323)
  full <-
    bagger(
      Species ~ .,
      data = iris,
      base_model = "C5.0",
      times = 3,
      control = control_bag(reduce = FALSE)
    )

  expect_true(length(full$model_df$model[[1]]$fit$control) > 1)
  expect_true(is.call(full$model_df$model[[1]]$fit$call))
  expect_true(nchar(full$model_df$model[[1]]$fit$output) > 10)

})

# ------------------------------------------------------------------------------

test_that('check C5 parsnip interface', {

  set.seed(4779)
  expect_error(
    class_mod <- bag_tree(min_n = 3) %>%
      set_engine("C5.0", times = 3) %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat),
    regexp = NA
  )
  expect_true(
    all(purrr::map_lgl(class_mod$fit$model_df$model, ~ inherits(.x, "model_fit")))
  )
  expect_true(
    all(purrr::map_lgl(class_mod$fit$model_df$model, ~ inherits(.x$fit, "C5.0")))
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

  set.seed(4779)
  expect_error(
    class_cost <- bag_tree(min_n = 3, class_cost = 2) %>%
      set_engine("C5.0", times = 3) %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat),
    regexp = NA
  )
  expect_true(
    all(purrr::map_lgl(class_cost$fit$model_df$model, ~ inherits(.x, "model_fit")))
  )
  expect_true(
    all(purrr::map_lgl(class_cost$fit$model_df$model, ~ inherits(.x$fit, "C5.0")))
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


  expect_output(print(bag_tree(min_n = 3)))
  expect_equal(update(bag_tree(), min_n = 3), bag_tree(min_n = 3))
  expect_equal(update(bag_tree(), cost_complexity = 3), bag_tree(cost_complexity = 3))
  expect_equal(update(bag_tree(), tree_depth = 3), bag_tree(tree_depth = 3))
  expect_equal(update(bag_tree(), class_cost = 3), bag_tree(class_cost = 3))

  expect_equal(class_cost(c(1, 5))$range$lower, 1)
  expect_equal(class_cost(c(1, 5))$range$upper, 5)

})

test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("bag_tree", "_pkgs")) %>%
      dplyr::filter(engine == "C5.0", mode == "classification") %>%
      dplyr::pull(pkg),
    list(c("C50", "baguette"))
  )

  expect_identical(
    get_from_env(paste0("bag_tree", "_pkgs")) %>%
      dplyr::filter(engine == "C5.0", mode == "regression") %>%
      dplyr::pull(pkg),
    list()
  )
})
