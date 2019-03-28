join_args <- function(default, others) {
  res <- default
  for (i in seq_along(others)) {
    res[[ names(others)[i] ]] <- others[[i]]
  }
  res
}
