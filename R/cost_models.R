cost_matrix <- function(x, lvl, truth_is_row = TRUE) {
  if (is.matrix(x)) {

  } else {
    if (length(lvl) != 2) {
      rlang::stop("`cost` can only be a scalar when there are two levels.")
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


cost_sens_cart_bagger <- function(rs, .control, extract, cost, ...) {
  # capture dots
  opts <-  rlang:lst(...)
  nms <- names(opts)

  cost <- cost_matrix(opts$cost)
  opts <- opts[[nms != "cost"]]
  nms <- names(opts)

  # attach cost matrix to parms = list(loss) but first
  # check ... for loss
  if (any(nms == "parms")) {
    opts$params$loss <-  cost
  } else {
    opts$params - list(loss = cost)
  }

  cart_bagger(rs = rs, .control = .control, extract = extract, !!!opts)
}
