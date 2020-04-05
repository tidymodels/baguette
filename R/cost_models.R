cost_matrix <- function(x, lvl, truth_is_row = TRUE) {
  if (is.matrix(x)) {

  } else {
    if (length(lvl) != 2) {
      rlang::abort("`cost` can only be a scalar when there are two levels.")
    } else {
      x0 <- x
      x <- matrix(1, ncol = 2, nrow = 2)
      x[2, 1] <- x0
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

cost_sens_cart_bagger <- function(rs, opt, control, cost, extract, ...) {
  # capture dots
  nms <- names(opt)
  lvl <- levels(rs$splits[[1]]$data$.outcome)

  cost <- cost_matrix(cost, lvl)

  # Attach cost matrix to parms = list(loss) but first
  # check existing options passed by user for loss
  if (any(nms == "parms")) {
    opt$parms$loss <-  cost
  } else {
    opt$parms <- list(loss = cost)
  }

  cart_bagger(rs = rs, opt = opt, control = control, extract = extract)
}



cost_sens_c5_bagger <- function(rs, opt, control, cost, extract, ...) {
  # capture dots
  nms <- names(opt)
  lvl <- levels(rs$splits[[1]]$data$.outcome)

  cost <- cost_matrix(cost, lvl, truth_is_row = FALSE)

  # Attach cost matrix to options
  opt$costs <-  cost

  c5_bagger(rs = rs, opt = opt, control = control, extract = extract)
}
