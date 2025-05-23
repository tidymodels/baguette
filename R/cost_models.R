cost_matrix <- function(x, lvl, truth_is_row = TRUE, call = rlang::caller_env()) {
  if (is.matrix(x)) {

  } else {
    if (length(lvl) != 2) {
      cli::cli_abort("{.arg cost} can only be a scalar when there are two
                     levels.", call = call)
    } else {
      x0 <- x
      x <- matrix(1, ncol = 2, nrow = 2)
      x[1, 2] <- x0
      diag(x) <- 0
      colnames(x) <- lvl
      rownames(x) <- lvl
    }
  }
  if (!truth_is_row) {
    x <- t(x)
  }
  x
}

cost_sens_cart_bagger <- function(rs, control, cost, ..., call = rlang::caller_env()) {

  # capture dots
  opt <- rlang::dots_list(...)
  nms <- names(opt)
  lvl <- levels(rs$splits[[1]]$data$.outcome)

  cost <- cost_matrix(cost, lvl, call = call)

  # Attach cost matrix to parms = list(loss) but first
  # check existing options passed by user for loss
  if (any(nms == "parms")) {
    opt$parms$loss <-  cost
  } else {
    opt$parms <- list(loss = cost)
  }

  cart_bagger(rs = rs, control = control, call = call, !!!opt)
}



cost_sens_c5_bagger <- function(rs, control, cost, ..., call = rlang::caller_env()) {

  # capture dots
  opt <- rlang::dots_list(...)
  nms <- names(opt)
  lvl <- levels(rs$splits[[1]]$data$.outcome)

  cost <- cost_matrix(cost, lvl, truth_is_row = FALSE, call = call)

  # Attach cost matrix to options
  opt$costs <-  cost

  c5_bagger(rs = rs, control = control, call = call, !!!opt)
}
