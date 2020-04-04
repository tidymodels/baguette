validate_args <- function(model, times, opt, control, cost, extract) {
  if (!is.character(model) || length(model) != 1) {
    rlang::abort("`model` should be a single character value.")
  }
  if (!(model %in% models)) {
    rlang::abort("`model` should be one of ", paste0("'", models, "'", collapse = ", "))
  }

  # ----------------------------------------------------------------------------

  if (!is.null(cost) & !(model %in% c("CART", "C5.0"))) {
    rlang::abort("`model` should be either 'CART' or 'C5.0'")
  }
  if (!is.null(cost)) {
    if (is.numeric(cost) && cost < 0) {
      rlang::abort("`cost` should be non-negative.")
    }
    if (is.matrix(cost)) {
      if (any(cost) < 0) {
        rlang::abort("`cost` should be non-negative.")
      }
    }
  }

  # ----------------------------------------------------------------------------

  if (!is.integer(times)) {
    rlang::abort("`times` must be an integer > 1.")
  }
  if (times < 1) {
    rlang::abort("`times` must be an integer > 1.")
  }

  # ----------------------------------------------------------------------------

  if (!is.null(opt) & !is.list(opt)) {
    rlang::abort("`opt` should be NULL or a named list.")
  }

  # TODO test for names, check args vs list

  # ----------------------------------------------------------------------------

  validate_control(.control)

  # ----------------------------------------------------------------------------

  if (!is.null(extract) && !is.function(extract)) {
    rlang::abort("`extract` should be NULL or a function.")
  }
  if (!is.null(extract)) {
    extract_nms <- names(formals(extract))
    if (length(extract_nms) != 2)
      rlang::abort("`extract` should have two arguments.")
    if (extract_nms[2] != "...")
      rlang::abort("The 2nd arg of `extract` should be `...`.")
  }

  # ----------------------------------------------------------------------------

  invisible(TRUE)

}

integer_B <- function(B) {
  if (is.numeric(B) & !is.integer(B))
    B <- as.integer(B)
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


    rlang::abort("All of the models failed. ", msg)
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
        rlang::abort("`type` should be either 'class' or 'prob'")
    } else {
      if (type != "numeric")
        rlang::abort("`type` should be 'numeric'")
    }
  }
  type
}

validate_importance <- function(x) {
  if (is.null(x)) {
    return(x)
  }

  if (!is_tibble(x)) {
    rlang::abort("Imprtance score results should be a tibble.")
  }

  exp_cols <- c("term", "value", "std.error", "used")
  if (!isTRUE(all.equal(exp_cols, names(x)))) {
    msg <- paste0("Importance columns should be: ",
                  paste0("'", exp_cols, "'", collapse = ", "),
                  "."
                  )
    rlang::abort(msg)
  }
  x
}

# ------------------------------------------------------------------------------

validate_control <- function(x) {
  if (!is.list(x)) {
    rlang::abort("The control object should be a list created by `bag_control()`.")
  }
  samps <- c("none", "down")

  if (length(x$var_imp) != 1 || !is.logical(x$var_imp)) {
    rlang::abort("`var_imp` should be a single logical value.")
  }
  if (length(x$allow_parallel) != 1 || !is.logical(x$allow_parallel)) {
    rlang::abort("`allow_parallel` should be a single logical value.")
  }
  if (length(x$sampling) != 1 || !is.character(x$sampling) || !any(samps == x$sampling)) {
    rlang::abort("`sampling` should be either 'none' or 'down'.")
  }
  if (!is.null(x$oob) && !inherits(x$oob, "function")) {
    rlang::abort("`oob` should be either NULL or the results of `yardstick::metric_set()`.")
  }
  x
}
