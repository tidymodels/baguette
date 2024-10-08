validate_args <- function(model, times, control, cost) {
  if (!is.character(model) || length(model) != 1) {
    cli::cli_abort("`base_model` should be a single character value.")
  }
  if (!(model %in% baguette_models)) {
    msg <- paste(
      "`base_model` should be one of ",
      paste0("'", baguette_models, "'", collapse = ", ")
    )
    cli::cli_abort(msg)
  }

  # ----------------------------------------------------------------------------

  if (!is.null(cost) & !(model %in% c("CART", "C5.0"))) {
    cli::cli_abort("`base_model` should be either 'CART' or 'C5.0'")
  }
  if (!is.null(cost)) {
    if (is.numeric(cost) && any(cost < 0)) {
      cli::cli_abort("`cost` should be non-negative.")
    }
  }

  # ----------------------------------------------------------------------------

  if (!is.integer(times)) {
    cli::cli_abort("`times` must be an integer > 1.")
  }
  if (times < 1) {
    cli::cli_abort("`times` must be an integer > 1.")
  }

  # ----------------------------------------------------------------------------

  validate_control(control)

  # ----------------------------------------------------------------------------

  invisible(TRUE)

}

integer_B <- function(B) {
  if (is.numeric(B) & !is.integer(B)) {
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

check_for_disaster <- function(x) {
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
      msg <- paste0("An example message was:\n  ", msg)
    } else msg <- ""


    cli::cli_abort(paste0("All of the models failed. ", msg))
  }
  x
}

# ------------------------------------------------------------------------------

check_type <- function(object, type) {
  if (is.null(type)) {
    if (object$base_model[2] == "classification") {
      type <- "class"
    } else {
      type <- "numeric"
    }
  } else {
    if (object$base_model[2] == "classification") {
      if (!(type %in% c("class", "prob")))
        cli::cli_abort("`type` should be either 'class' or 'prob'")
    } else {
      if (type != "numeric")
        cli::cli_abort("`type` should be 'numeric'")
    }
  }
  type
}

validate_importance <- function(x) {
  if (is.null(x)) {
    return(x)
  }

  if (!is_tibble(x)) {
    cli::cli_abort("Imprtance score results should be a tibble.")
  }

  exp_cols <- c("term", "value", "std.error", "used")
  if (!isTRUE(all.equal(exp_cols, names(x)))) {
    msg <- paste0("Importance columns should be: ",
                  paste0("'", exp_cols, "'", collapse = ", "),
                  "."
                  )
    cli::cli_abort(msg)
  }
  x
}

# ------------------------------------------------------------------------------

validate_control <- function(x) {
  if (!is.list(x)) {
    cli::cli_abort("The control object should be a list created by `control_bag()`.")
  }
  samps <- c("none", "down")

  if (length(x$var_imp) != 1 || !is.logical(x$var_imp)) {
    cli::cli_abort("`var_imp` should be a single logical value.")
  }
  if (length(x$allow_parallel) != 1 || !is.logical(x$allow_parallel)) {
    cli::cli_abort("`allow_parallel` should be a single logical value.")
  }
  if (length(x$sampling) != 1 || !is.character(x$sampling) || !any(samps == x$sampling)) {
    cli::cli_abort("`sampling` should be either 'none' or 'down'.")
  }
  if (length(x$reduce) != 1 || !is.logical(x$reduce)) {
    cli::cli_abort("`reduce` should be a single logical value.")
  }
  if (!is.null(x$extract) && !is.function(x$extract)) {
    cli::cli_abort("`extract` should be NULL or a function.")
  }

  x
}
