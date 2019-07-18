library(testthat)

context("model rules")

# ------------------------------------------------------------------------------

test_that('check cubist opt', {

  check_unbiased <- function(x, ...) {
    x$control$unbiased
  }
  mod_1 <-
    bagger(
      Ozone ~ .,
      data = airquality,
      model = "model_rules",
      opt = list(unbiased = TRUE),
      extract = check_unbiased
    )
  expect_true(all(unlist(mod_1$model_df$extras)))
  expect_true(!is.null(mod_1$imp))

  mod_2 <-
    bagger(
      Ozone ~ .,
      data = airquality,
      model = "model_rules",
      control = bag_control(var_imp = TRUE)
    )
  expect_true(inherits(mod_2$imp, "tbl_df"))
})

# ------------------------------------------------------------------------------

test_that('check cubist OOB', {
  ms_3 <- metric_set(rsq)
  mod_3 <-
    bagger(
      Sepal.Width ~ .,
      data = iris,
      model = "model_rules",
      oob = ms_1
    )
  expect_true(all(mod_3$oob$.metric == "rsq"))
  expect_true(all(!is.na(mod_3$oob$mean)))

})


# ------------------------------------------------------------------------------

test_that('cubist predictions', {

  # airquality_na <- airquality
  # airquality_na$Wind[seq(1, 150, by = 10)] <- NA
  # holdout <- seq(1, 150, by = 5)
  # mod_4 <-
  #   bagger(
  #     Ozone ~ .,
  #     data = airquality_na[-holdout, ],
  #     model = "model_rules",
  #     control = bag_control(oob = metric_set(rsq))
  #   )
  #
  # pred_4 <- predict(mod_4, airquality_na[holdout, -1])
  # expect_equal(nrow(pred_4), length(holdout))

  holdout <- seq(1, 150, by = 5)
  set.seed(5145)
  mod_4 <-
    bagger(
      Ozone ~ .,
      data = airquality[-holdout, ],
      model = "model_rules",
      control = bag_control(oob = metric_set(rsq))
    )

  # pred_4 <- predict(mod_4, airquality[holdout, -1])
  # expect_equal(nrow(pred_4), length(holdout))

})

