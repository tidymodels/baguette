#' Bagging functions
#'
#' General suite of bagging functions for several models.
#'
#' @param x A data frame, matrix, or recipe (depending on the method being used.)
#' @param y A numeric or factor vector of outcomes. Categorical outcomes (i.e
#'  classes) should be represented as factors, not integers.
#' @param formula  An object of class "formula" (or one that can be coerced to
#'  that class): a symbolic description of the model to be fitted. Note that
#'  this package does not support multivariate outcomes and that, if some
#'  predictors are factors, dummy variables will not be created.
#' @param data A data frame containing the variables used in the formula or
#'  recipe.
#' @param model A single character value for the model being bagged. Possible
#'  values are "CART", "MARS", "C5.0" (classification only), and "model_rules"
#'  (regression only).
#' @param B A single integer greater than 1 for the maximum number of bootstrap
#'  samples/ensemble members (some model fits might fail).
#' @param opt A named list (or NULL) of arguments to pass to the underlying
#'  model function. A list of possible arguments per model are given in Details.
#' @param var_imp A logical: should variable importance scores be calculated?
#' @param oob A metric set created by [yardstick::metric_set()] or NULL. If not
#'  NULL, then the out-of-bag samples are used to estimate model performance.
#' @param extract A function (or NULL) that can extract model-related aspects
#'  of each ensemble member. See Details below.
#' @param ... Optional arguments to pass to the `extract` function.
#' @details TBD
#' @examples
#' mars_reg <- bagger(Sepal.Width ~ ., data = iris, model = "MARS", var_imp = TRUE)
#'
#' library(AmesHousing)
#' ames <- make_ames()
#' # These take a while:
#'
#' \dontrun{
#' ames_mars <- bagger(log10(Sale_Price) ~ ., data = ames, model = "MARS", var_imp = TRUE)
#' ames_rules <- bagger(log10(Sale_Price) ~ ., data = ames, model = "model_rules", var_imp = TRUE)
#' }
#' @export
bagger <- function(x, ...) {
  UseMethod("bagger")
}

#' @export
#' @rdname bagger
bagger.default <- function(x, ...) {
  stop("`bagger()` is not defined for a '", class(x)[1], "'.",
       call. = FALSE)
}

# XY method - data frame

#' @export
#' @rdname bagger
bagger.data.frame <-
  function(x,
           y,
           model = "CART",
           B = 10L,
           opt = NULL,
           var_imp = FALSE,
           oob = NULL,
           extract = NULL,
           ...) {
    B <- integer_B(B)
    seed <- sample.int(10^5, 1)
    validate_args(model, B, opt, var_imp, oob, extract)

    processed <- hardhat::mold(x, y)
    bagger_bridge(processed, model, seed, B, opt, var_imp, oob, extract, ...)
  }

# XY method - matrix

#' @export
#' @rdname bagger
bagger.matrix <-
  function(x,
           y,
           model = "CART",
           B = 10L,
           opt = NULL,
           var_imp = FALSE,
           oob = NULL,
           extract = NULL,
           ...) {
    B <- integer_B(B)
    seed <- sample.int(10^5, 1)
    validate_args(model, B, opt, var_imp, oob, extract)

    processed <- hardhat::mold(x, y)
    bagger_bridge(processed, model, seed, B, opt, var_imp, oob, extract, ...)
  }

# Formula method

#' @export
#' @rdname bagger
bagger.formula <-
  function(formula,
           data,
           model = "CART",
           B = 10L,
           opt = NULL,
           var_imp = FALSE,
           oob = NULL,
           extract = NULL,
           ...) {
    B <- integer_B(B)
    seed <- sample.int(10^5, 1)
    validate_args(model, B, opt, var_imp, oob, extract)

    bp <- hardhat::default_formula_blueprint(indicators = FALSE)
    processed <- hardhat::mold(formula, data, blueprint = bp)
    bagger_bridge(processed, model, seed, B, opt, var_imp, oob, extract, ...)
  }

# Recipe method

#' @export
#' @rdname bagger
bagger.recipe <-
  function(x,
           data,
           model = "CART",
           B = 10L,
           opt = NULL,
           var_imp = FALSE,
           oob = NULL,
           extract = NULL,
           ...) {
    B <- integer_B(B)
    seed <- sample.int(10^5, 1)
    validate_args(model, B, opt, var_imp, oob, extract)

    processed <- hardhat::mold(x, data)
    bagger_bridge(processed, model, seed, B, opt, var_imp, oob, extract, ...)
  }

# ------------------------------------------------------------------------------

#' @export
print.bagger <- function(x, ...) {
  cat("Bagged ", x$model[1], " (", x$model[2], " with ",
      nrow(x$model_df), " members)\n", sep = "")

  if (!is.null(x$opt)) {
    cat("Additional model options:\n")
    opt_chr <- paste0("  ", names(x$opt), ": ", format(x$opt))
    cat(opt_chr, sep = "\n")
  }

  if (!is.null(x$imp)) {
    cat("\nVariable importance scores include:\n\n")
    print(x$imp)
    cat("\n")
  }

  invisible(x)
}


