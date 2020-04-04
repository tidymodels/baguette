validate_args <- function(base_model, times, .control, extract) {
  if (!is.character(base_model) || length(base_model) != 1) {
    stop("`base_model` should be a single character value.", call. = FALSE)
  }
  if (!(base_model %in% models)) {
    stop("`base_model` should be one of ", paste0("'", models, "'", collapse = ", "),
         call. = FALSE)
  }

  # ----------------------------------------------------------------------------

  if (!is.integer(times)) {
    stop("`times` must be an integer > 1.", call. = FALSE)
  }
  if (times < 1) {
    stop("`times` must be an integer > 1.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------

  # TODO test for names, check args vs list

  # ----------------------------------------------------------------------------

  validate_control(.control)

  # ----------------------------------------------------------------------------

  if (!is.null(extract) && !is.function(extract)) {
    stop("`extract` should be NULL or a function.", call. = FALSE)
  }
  if (!is.null(extract)) {
    extract_nms <- names(formals(extract))
    if (length(extract_nms) != 2)
      stop("`extract` should have two arguments.", call. = FALSE)
    if (extract_nms[2] != "...")
      stop("The 2nd arg of `extract` should be `...`.", call. = FALSE)
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


    stop("All of the models failed. ", msg, call. = FALSE)
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
        stop("`type` should be either 'class' or 'prob'", call. = FALSE)
    } else {
      if (type != "numeric")
        stop("`type` should be 'numeric'", call. = FALSE)
    }
  }
  type
}

validate_importance <- function(x) {
  if (is.null(x)) {
    return(x)
  }

  if (!is_tibble(x)) {
    stop("Imprtance score results should be a tibble.", call. = FALSE)
  }

  exp_cols <- c("term", "value", "std.error", "used")
  if (!isTRUE(all.equal(exp_cols, names(x)))) {
    msg <- paste0("Importance columns should be: ",
                  paste0("'", exp_cols, "'", collapse = ", "),
                  "."
                  )
    stop(msg, call. = FALSE)
  }
  x
}

# ------------------------------------------------------------------------------

validate_control <- function(x) {
  if (!is.list(x)) {
    stop("The '.control' object should be a list created by `bag_control()`.",
         call. = FALSE)
  }
  samps <- c("none", "down")

  if (length(x$var_imp) != 1 || !is.logical(x$var_imp)) {
    stop("`var_imp` should be a single logical value.", call. = FALSE)
  }
  if (length(x$allow_parallel) != 1 || !is.logical(x$allow_parallel)) {
    stop("`allow_parallel` should be a single logical value.", call. = FALSE)
  }
  if (length(x$sampling) != 1 || !is.character(x$sampling) || !any(samps == x$sampling)) {
    stop("`sampling` should be either 'none' or 'down'.", call. = FALSE)
  }
  if (!is.null(x$oob) && !inherits(x$oob, "function")) {
    stop("`oob` should be either NULL or the results of `yardstick::metric_set()`.",
         call. = FALSE)
  }
  x
}
