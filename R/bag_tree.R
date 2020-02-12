# Prototype parsnip code for decision trees

#' General Interface for Decision Tree Models
#'
#' `bag_tree()` is a way to generate a _specification_ of a model
#'  before fitting and allows the model to be created using
#'  different packages in R. The main arguments for the
#'  model are:
#' \itemize{
#'   \item \code{cost_complexity}: The cost/complexity parameter (a.k.a. `Cp`)
#'    used by CART models (`rpart` only).
#'   \item \code{tree_depth}: The _maximum_ depth of a tree (`rpart`).
#'   \item \code{min_n}: The minimum number of data points in a node
#'   that are required for the node to be split further.
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
#' @param cost_complexity A positive number for the the cost/complexity
#'   parameter (a.k.a. `Cp`) used by CART models (`rpart` only).
#' @param tree_depth An integer for maximum depth of the tree.
#' @param min_n An integer for the minimum number of data points
#'  in a node that are required for the node to be split further.
#' @details
#' The model can be created using the `fit()` function using the
#'  following _engines_:
#' \itemize{
#' \item \pkg{R}:  `"rpart"` (the default) or `"C5.0"` (classification only)
#' }
#'
#' Note that, for `rpart` models, but `cost_complexity` and
#'  `tree_depth` can be both be specified but the package will give
#'  precedence to `cost_complexity`. Also, `tree_depth` values
#'  greater than 30 `rpart` will give nonsense results on 32-bit
#'  machines.
#' @importFrom purrr map_lgl
#' @seealso [[fit()]
#' @examples
#' bag_tree(tree_depth = 5) %>% set_mode("classification")
#' @export

bag_tree <-
  function(mode = "unknown", cost_complexity = 0, tree_depth = NULL, min_n = 2) {

    args <- list(
      cost_complexity   = enquo(cost_complexity),
      tree_depth  = enquo(tree_depth),
      min_n  = enquo(min_n)
    )

    new_model_spec(
      "bag_tree",
      args = args,
      eng_args = NULL,
      mode = mode,
      method = NULL,
      engine = NULL
    )
  }

#' @export
print.bag_tree <- function(x, ...) {
  cat("Bagged Decision Tree Model Specification (", x$mode, ")\n\n", sep = "")
  model_printer(x, ...)

  if (!is.null(x$method$fit$args)) {
    cat("Model fit template:\n")
    print(show_call(x))
  }
  invisible(x)
}

# ------------------------------------------------------------------------------

#' @export
#' @param object A bagged tree model specification.
#' @examples
#' model <- bag_tree(cost_complexity = 10, min_n = 3)
#' model
#' update(model, cost_complexity = 1)
#' update(model, cost_complexity = 1, fresh = TRUE)
#' @method update bag_tree
#' @rdname bag_tree
#' @export
update.bag_tree <-
  function(object,
           parameters = NULL,
           cost_complexity = NULL, tree_depth = NULL, min_n = NULL,
           fresh = FALSE, ...) {
    update_dot_check(...)

    if (!is.null(parameters)) {
      parameters <- check_final_param(parameters)
    }
    args <- list(
      cost_complexity   = enquo(cost_complexity),
      tree_depth  = enquo(tree_depth),
      min_n  = enquo(min_n)
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
      "bag_tree",
      args = object$args,
      eng_args = object$eng_args,
      mode = object$mode,
      method = NULL,
      engine = object$engine
    )
  }


# ------------------------------------------------------------------------------

check_args.bag_tree <- function(object) {
  if (object$engine == "C5.0" && object$mode == "regression")
    stop("C5.0 is classification only.", call. = FALSE)
  invisible(object)
}

# ------------------------------------------------------------------------------

tree_wrapper <-
  function(x,
           y,
           cost_complexity = 0,
           tree_depth = NULL,
           min_n = 2,
           ...) {
    opts <- list(...)
    bagger(x = x, y = y, model = "CART", opts = opts, ...)
  }
