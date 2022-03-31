library(testthat)
library(rlang)

context("MARS models")

# ------------------------------------------------------------------------------

data("two_class_dat", package = "modeldata")

# ------------------------------------------------------------------------------

test_that('check mars opt', {
  set.seed(36323)
  check_pruning <- function(x, ...) {
    rlang::eval_tidy(x$call$pmethod) == "backward"
  }
  mod_1 <-
    bagger(
      mpg ~ .,
      data = mtcars,
      base_model = "MARS",
      control = control_bag(var_imp = FALSE, extract = check_pruning),
      pmethod = "backward"
    )
  expect_true(all(unlist(mod_1$model_df$extras)))
  expect_true(is.null(mod_1$imp))

  check_folds <- function(x, ...) {
    rlang::eval_tidy(x$call$pmethod) == "backward" &
      rlang::eval_tidy(x$call$nfold) == "5" &
      is.null(x$glm.coefficients)
  }
  mod_2 <-
    bagger(
      mpg ~ .,
      data = mtcars,
      base_model = "MARS",
      control = control_bag(var_imp = TRUE, extract = check_folds),
      nfold = 5,
      pmethod = "backward"
    )
  expect_true(all(unlist(mod_2$model_df$extras)))
  expect_true(inherits(mod_2$imp, "tbl_df"))

  check_classif <- function(x, ...) {
    !is.null(x$glm.coefficients)
  }

  # For correct random numbers
  if (compareVersion(as.character(getRversion()), "3.6.0") > 0) {
    expect_warning(RNGkind(sample.kind = "Rounding"))
  }
  set.seed(2234)
  expect_warning(
    mod_3 <-
      bagger(
        Class ~ .,
        data = two_class_dat,
        base_model = "MARS",
        control = control_bag(var_imp = TRUE, extract = check_classif)
      ),
    "fitted probabilities numerically 0"
  )
  expect_true(all(unlist(mod_3$model_df$extras)))
  expect_true(inherits(mod_3$imp, "tbl_df"))
})

# ------------------------------------------------------------------------------

test_that('check model reduction', {
  set.seed(36323)
  reduced <-
    bagger(
      mpg ~ .,
      data = mtcars,
      base_model = "MARS",
      times = 3
    )
  expect_false(is.matrix(reduced$model_df$model[[1]]$fit$y))
  expect_equal(reduced$model_df$model[[1]]$fit$call, rlang::call2("dummy_call"))
  expect_equal(reduced$model_df$model[[1]]$fit$residuals, numeric(0))

  set.seed(36323)
  full <-
    bagger(
      mpg ~ .,
      data = mtcars,
      base_model = "MARS",
      times = 3,
      control = control_bag(reduce = FALSE)
    )

  expect_true(is.matrix(full$model_df$model[[1]]$fit$y))
  expect_true(is.call(full$model_df$model[[1]]$fit$call))
  expect_true(is.matrix(full$model_df$model[[1]]$fit$residuals))

})

# ------------------------------------------------------------------------------

test_that('check MARS parsnip interface', {
  set.seed(4779)
  expect_error(
    reg_mod <- bag_mars(num_terms = 5, prod_degree = 2) %>%
      set_engine("earth", times = 3) %>%
      set_mode("regression") %>%
      fit(mpg ~ ., data = mtcars),
    regexp = NA
  )
  expect_true(
    all(purrr::map_lgl(reg_mod$fit$model_df$model, ~ inherits(.x, "model_fit")))
  )
  expect_true(
    all(purrr::map_lgl(reg_mod$fit$model_df$model, ~ inherits(.x$fit, "earth")))
  )
  expect_error(
    reg_mod_pred <- predict(reg_mod, mtcars[1:5, -1]),
    regexp = NA
  )
  expect_true(tibble::is_tibble(reg_mod_pred))
  expect_equal(nrow(reg_mod_pred), 5)
  expect_equal(names(reg_mod_pred), ".pred")

  # ----------------------------------------------------------------------------

  set.seed(4779)
  expect_error(
    reg_class <- bag_mars(num_terms = 5, prod_degree = 2) %>%
      set_engine("earth", times = 3) %>%
      set_mode("classification") %>%
      fit(Class ~ ., data = two_class_dat),
    regexp = NA
  )
  expect_true(
    all(purrr::map_lgl(reg_class$fit$model_df$model, ~ inherits(.x, "model_fit")))
  )
  expect_true(
    all(purrr::map_lgl(reg_class$fit$model_df$model, ~ inherits(.x$fit, "earth")))
  )
  expect_error(
    reg_class_pred <- predict(reg_class, two_class_dat[1:5, -3]),
    regexp = NA
  )
  expect_true(tibble::is_tibble(reg_class_pred))
  expect_equal(nrow(reg_class_pred), 5)
  expect_equal(names(reg_class_pred), ".pred_class")

  expect_error(
    reg_class_prob <- predict(reg_class, two_class_dat[1:5, -3], type = "prob"),
    regexp = NA
  )
  expect_true(tibble::is_tibble(reg_class_prob))
  expect_equal(nrow(reg_class_prob), 5)
  expect_equal(names(reg_class_prob), c(".pred_Class1", ".pred_Class2"))

  expect_output(print(bag_mars(num_terms = 3)))
  expect_equal(update(bag_mars(), num_terms = 3), bag_mars(num_terms = 3))
  expect_equal(update(bag_mars(), prod_degree = 1), bag_mars(prod_degree = 1))
  expect_equal(update(bag_mars(), prune_method = "none"), bag_mars(prune_method = "none"))
})

test_that('mode specific package dependencies', {
  expect_identical(
    get_from_env(paste0("bag_mars", "_pkgs")) %>%
      dplyr::filter(engine == "earth", mode == "classification") %>%
      dplyr::pull(pkg),
    list(c("earth", "baguette"))
  )

  expect_identical(
    get_from_env(paste0("bag_mars", "_pkgs")) %>%
      dplyr::filter(engine == "earth", mode == "regression") %>%
      dplyr::pull(pkg),
    list(c("earth", "baguette"))
  )
})



test_that('case weights', {
  set.seed(1)
  wts <- runif(nrow(mtcars))

  expect_error({
    set.seed(1)
    wts_fit <- bagger(mpg ~ ., data = mtcars, weights = wts, base_model = "MARS")
  },
  regexp = NA
  )

  set.seed(1)
  fit <- bagger(mpg ~ ., data = mtcars, base_model = "MARS")
  expect_true(!identical(wts_fit$imp, fit$imp))
})


