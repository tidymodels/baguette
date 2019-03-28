#' @importFrom tibble is_tibble

new_bagger <- function(model_df, imp, opt, model, blueprint) {

  if (!is_tibble(model_df)) {
    stop("`model_df` should be a tibble.", call. = FALSE)
  }


  if (!is_tibble(imp) & !is.null(imp)) {
    stop("`imp` should be a tibble.", call. = FALSE)
  }
  if (!is.list(opt) & !is.null(opt)) {
    stop("`opt` should be a list or NULL", call. = FALSE)
  }
  hardhat::new_model(
    model_df = model_df,
    imp = imp,
    opt = opt,
    model = model,
    blueprint = blueprint,
    class = "bagger"
  )
}
