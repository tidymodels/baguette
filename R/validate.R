validate_args <- function(model, B, opt, var_imp, oob, extract) {
  if (!is.character(model) || length(model) != 1) {
    stop("`model` should be a single character value.", call. = FALSE)
  }
  if (!(model %in% models)) {
    stop("`model` should be one of ", paste0("'", models, "'", collapse = ", "),
         call. = FALSE)
  }

  # ----------------------------------------------------------------------------

  if (!is.integer(B)) {
    stop("`B` must be an integer > 1.", call. = FALSE)
  }
  if (B < 1) {
    stop("`B` must be an integer > 1.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------

  if (!is.null(opt) & !is.list(opt)) {
    stop("`opt` should be NULL or a named list.", call. = FALSE)
  }

  # TODO test for names, check args vs list

  # ----------------------------------------------------------------------------

  if (!is.logical(var_imp) || length(var_imp) > 1) {
    stop("`var_imp` should be single logical.", call. = FALSE)
  }

  # ----------------------------------------------------------------------------

  if (!is.null(oob) & !is.function(oob)) {
    stop("`oob` should be a function created by yardstick::metric_set.",
          call. = FALSE)
  }

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

validate_y_type <- function(model, outcomes) {
  hardhat::validate_outcomes_is_univariate(outcomes)

  if (model == "X") {
    hardhat::validate_outcomes_is_binary(outcomes)
  }

  if (model == "model_rules") {
    if (!is.numeric(outcomes[[1]]))
      stop("Outcome data must be numeric for model rules.", call. = FALSE)
    #hardhat::validate_outcomes_is_numeric(outcomes)
  }
  if (model == "C5.0") {
    if (!is.factor(outcomes[[1]]))
      stop("Outcome data must be a factor for C5.0.", call. = FALSE)
    #hardhat::validate_outcomes_is_factor(outcomes)
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
  invisible(TRUE)
}

# ------------------------------------------------------------------------------

check_type <- function(object, type) {
  if (is.null(type)) {
    if (object$model[2] == "classification") {
      type <- "class"
    } else {
      type <- "numeric"
    }
  } else {
    if (object$model[2] == "classification") {
      if (!(type %in% c("class", "prob")))
        stop("`type` should be either 'class' or 'prob'", call. = FALSE)
    } else {
      if (type != "numeric")
        stop("`type` should be 'numeric'", call. = FALSE)
    }
  }
  type
}
