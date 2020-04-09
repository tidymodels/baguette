#' General Interface for Bagged MARS Models
#'
#' `bag_mars()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R. The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{num_terms}: The number of features that will be retained in the
#'    final model.
#'   \item \code{prod_degree}: The highest possible degree of interaction between
#'    features. A value of 1 indicates and additive model while a value of 2
#'    allows, but does not guarantee, two-way interactions between features.
#'   \item \code{prune_method}: The type of pruning. Possible values are listed
#'    in `?earth`.
#' }
#' These arguments are converted to their specific names at the
#'  time that the model is fit. Other options and argument can be
#'  set using `set_engine()`. If left to their defaults
#'  here (`NULL`), the values are taken from the underlying model
#'  functions. If parameters need to be modified, `update()` can be used
#'  in lieu of recreating the object from scratch.
#'
#' @param mode A single character string for the type of model.
#'  Possible values for this model are "unknown", "regression", or
#'  "classification".
#' @param num_terms The number of features that will be retained in the
#'    final model, including the intercept.
#' @param prod_degree The highest possible interaction degree.
#' @param prune_method The pruning method.
#' @details The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"earth"`  (the default)
#' }
#'
#' @importFrom purrr map_lgl
#' @examples
#' library(parsnip)
#'
#' set.seed(7396)
#' bag_mars(num_terms = 7) %>%
#'   set_mode("regression") %>%
#'   set_engine("earth", times = 3) %>%
#'   fit(mpg ~ ., data = mtcars)
#' @export

bag_mars <-
  function(mode = "unknown", num_terms = NULL, prod_degree = NULL, prune_method = NULL) {

    args <- list(
      num_terms   = enquo(num_terms),
      prod_degree  = enquo(prod_degree),
      prune_method  = enquo(prune_method)
    )

    new_model_spec(
      "bag_mars",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.bag_mars <- function(x, ...) {
  cat("Bagged MARS Model Specification (", x$mode, ")\n\n", sep = "")
  parsnip::model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(parsnip::show_call(x))
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' @export
#' @param object A bagged MARS model specification.
#' @param parameters A 1-row tibble or named list with _main_
#'  parameters to update. If the individual arguments are used,
#'  these will supersede the values in `parameters`. Also, using
#'  engine arguments in this object will result in an error.
#' @param ... Not used for `update()`.
#' @param fresh A logical for whether the arguments should be
#'  modified in-place of or replaced wholesale.
#' @examples
#'
#'
#' model <- bag_mars(num_terms = 10, prune_method = "none")
#' model
#' update(model, num_terms = 2)
#' update(model, num_terms = 2, fresh = TRUE)
#' @method update bag_mars
#' @rdname bag_mars
#' @export
update.bag_mars <-
  function(object,
           parameters = NULL,
           num_terms = NULL, prod_degree = NULL, prune_method = NULL,
           fresh = FALSE, ...) {
    update_dot_check(...)

    if (!is.null(parameters)) {
      parameters <- parsnip::check_final_param(parameters)
    }
    args <- list(
      num_terms   = enquo(num_terms),
      prod_degree  = enquo(prod_degree),
      prune_method  = enquo(prune_method)
    )

    args <- update_main_parameters(args, parameters)

    if (fresh) {
      object$args <- args
    } else {
      null_args <- map_lgl(args, null_value)
      if (any(null_args))
        args <- args[!null_args]
      if (length(args) > 0)
        object$args[names(args)] <- args
    }

    new_model_spec(
      "bag_mars",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }

