#' @importFrom tibble is_tibble

new_bagger <- function(model_df, imp, oob, opt, model, blueprint) {

  if (!is_tibble(model_df)) {
    stop("`model_df` should be a tibble.", call. = FALSE)
  }


  if (!is_tibble(imp) & !is.null(imp)) {
    stop("`imp` should be a tibble.", call. = FALSE)
  }
  if (!is.list(opt) & !is.null(opt)) {
    stop("`opt` should be a list or NULL", call. = FALSE)
  }
  if (!is_tibble(oob) & !is.null(oob)) {
    stop("`oob` should be a tibble.", call. = FALSE)
  }

  if (!is.null(oob)) {
    exp_nm <- c(".metric", "mean", "stdev", "n")
    if (length(setdiff(exp_nm, names(oob))) > 0 |
        length(setdiff(names(oob), exp_nm)) > 0 ) {
      stop(
        "`oob` should have columns ",
        paste0("'", exp_nm, "'", collapse = ","),
        ".", call. = FALSE
      )
    }
  }

  if (is.numeric(blueprint$ptypes$outcomes[[1]])) {
    mod_mode <- "regression"
  } else {
    mod_mode <- "classification"
  }

  hardhat::new_model(
    model_df = model_df,
    imp = imp,
    opt = opt,
    oob = oob,
    model = c(model[1], mod_mode),
    blueprint = blueprint,
    class = "bagger"
  )
}
