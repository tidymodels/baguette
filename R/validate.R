#' @include import-standalone-types-check.R
validate_args <- function(model, times, control, cost, call = rlang::caller_env()) {
  model <- rlang::arg_match(model, baguette_models, error_arg = "base_model",
                            error_call = call)

  if (!is.null(cost) & !(model %in% c("CART", "C5.0"))) {
    cli::cli_abort("When using misclassification costs, {.arg base_model} should
                    be either {.val CART} or {.val C5.0}, not
                   {obj_type_friendly(model)}.", call = call)
  }
  if (!is.matrix(cost)) {
    check_number_decimal(cost, allow_null = TRUE, min = 0, call = call)
  } else {
    is_sq <- nrow(cost) == ncol(cost)
    if (!is.numeric(cost) || !is_sq) {
      cli::cli_abort("If {.arg cost} is a matrix, is must be numeric and square.",
                     call = call)
    }
  }

  check_number_whole(times, min = 2, call = call)

  validate_control(control, call = call)

  invisible(TRUE)

}

integer_B <- function(B) {
  if (is.double(B)) {
    B <- as.integer(B)
  }
  B
}

# ------------------------------------------------------------------------------

validate_y_type <- function(base_model, outcomes) {
  hardhat::validate_outcomes_are_univariate(outcomes)

  if (base_model == "C5.0") {
    hardhat::validate_outcomes_are_factors(outcomes)
  }

}

# ------------------------------------------------------------------------------

model_failure <- function(x) {
  if (inherits(x, "model_fit")) {
    res <- inherits(x$fit, "try-error")
  } else {
    res <- inherits(x, "try-error")
  }
  res
}

check_for_disaster <- function(x, call = rlang::caller_env()) {
  x <- dplyr::mutate(x, passed = !purrr::map_lgl(model, model_failure))

  if (sum(x$passed) == 0) {
    if (inherits(x$model[[1]], "try-error")) {
      msg <- as.character(x$model[[1]])
    } else {
      if (inherits(x$model[[1]], "model_fit")) {
        msg <- as.character(x$model[[1]]$fit)
      } else msg <- NA
    }

    if (!is.na(msg)) {
      # escape any brackets in the error message
      msg <- cli::format_error("{msg}")
      cli::cli_abort(
        c("All of the models failed. Example:", "x" = "{msg}"),
        call = call
      )
    } else {
      cli::cli_abort("All of the models failed.", call = call)
    }
  }
  x
}

# ------------------------------------------------------------------------------

check_type <- function(object, type, call = rlang::caller_env()) {
  model_type <- object$base_model[2]
  model_modes <- parsnip::get_from_env("modes")
  if (is.null(type)) {
    if (model_type == "classification") {
      type <- "class"
    } else if (model_type == "regression") {
      type <- "numeric"
    }
  } else {
    if (model_type == "classification") {
      type <- rlang::arg_match(type, c("class", "prob"), error_call = call)
    } else if (model_type == "regression") {
      type <- rlang::arg_match(type, c("numeric"), error_call = call)
    } else {
      cli::cli_abort("Model mode {.val {model_type}} is not allowed
                     Possible values are {.or {.val {model_modes}}}.",
                     call = call)
    }
  }
  type
}

validate_importance <- function(x, call = rlang::caller_env()) {
  if (is.null(x)) {
    return(x)
  }

  if (!is_tibble(x)) {
    cli::cli_abort("Imprtance score results should be a tibble, not
                   {obj_type_friendly(x)}.", call = call)
  }

  exp_cols <- c("term", "value", "std.error", "used")
  if (!isTRUE(all.equal(exp_cols, names(x)))) {
    cli::cli_abort("Importance columns should be: {.val {exp_cols}}.", call = call)
  }
  x
}

# ------------------------------------------------------------------------------

validate_control <- function(x, call = rlang::caller_env()) {
  if (!is.list(x)) {
    cli::cli_abort("The control object should be a list created by
                   {.fn control_bag}, not {obj_type_friendly(x)}.", call = call)
  }

  check_bool(x$var_imp, arg = "var_imp", call = call)
  check_bool(x$allow_parallel, arg = "allow_parallel", call = call)
  x$sampling <- rlang::arg_match0(x$sampling, c("none", "down"),
                                  arg_nm = "sampling", error_call = call)
  check_bool(x$reduce, arg = "reduce", call = call)
  check_function(x$extract, allow_null = TRUE, arg = "extract", call = call)

  x
}

# ------------------------------------------------------------------------------

validate_case_weights <- function(weights, data, call = rlang::caller_env()) {
  if (is.null(weights)) {
    return(invisible(NULL))
  }
  n <- nrow(data)
  if (!is.vector(weights) || !is.numeric(weights) || length(weights) != n ||
      any(weights < 0)) {
    cli::cli_abort("{.arg weights} should be a non-negative numeric vector
                    with the same size as the data, not
                   {obj_type_friendly(weights)}.", call = call)
  }
  invisible(NULL)
}

