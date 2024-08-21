new_bagger <- function(model_df, imp, control, cost, base_model, blueprint) {

  if (!is_tibble(model_df)) {
    cli::cli_abort("`model_df` should be a tibble.")
  }

  if (is.numeric(blueprint$ptypes$outcomes[[1]])) {
    mod_mode <- "regression"
  } else {
    mod_mode <- "classification"
  }

  hardhat::new_model(
    model_df = model_df,
    control = control,
    cost = cost,
    imp = imp,
    base_model = c(base_model[1], mod_mode),
    blueprint = blueprint,
    class = "bagger"
  )
}
