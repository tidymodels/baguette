# TODO add ... in here somewhere

new_bagger <- function(model_df, imp, oob, control, .cost, opt, model, blueprint) {

  if (!is_tibble(model_df)) {
    rlang::abort("`model_df` should be a tibble.")
  }

  if (!is.list(opt) & !is.null(opt)) {
    rlang::abort("`opt` should be a list or NULL.")
  }

  if (!is_tibble(oob) & !is.null(oob)) {
    rlang::abort("`oob` should be a tibble.")
  }

  if (!is.null(oob)) {
    exp_nm <- c(".metric", ".estimator", ".estimate")
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
    control = control,
    .cost = .cost,
    imp = imp,
    oob = oob,
    base_model = c(base_model[1], mod_mode),
    blueprint = blueprint,
    class = "bagger"
  )
}
