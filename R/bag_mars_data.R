# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov start

make_bag_mars <- function() {

  parsnip::set_model_engine("bag_mars", "classification", "earth")
  parsnip::set_model_engine("bag_mars", "regression", "earth")
  parsnip::set_dependency("bag_mars", "earth", "earth", mode = "classification")
  parsnip::set_dependency("bag_mars", "earth", "earth", mode = "regression")
  parsnip::set_dependency("bag_mars", "earth", "baguette", mode = "classification")
  parsnip::set_dependency("bag_mars", "earth", "baguette", mode = "regression")

  parsnip::set_model_arg(
    model = "bag_mars",
    eng = "earth",
    parsnip = "prod_degree",
    original = "degree",
    func = list(pkg = "dials", fun = "prod_degree"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "bag_mars",
    eng = "earth",
    parsnip = "prune_method",
    original = "pmethod",
    func = list(pkg = "dials", fun = "prune_method"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "bag_mars",
    eng = "earth",
    parsnip = "num_terms",
    original = "nprune",
    func = list(pkg = "dials", fun = "num_terms", range = c(2, 5)),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "bag_mars",
    eng = "earth",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "baguette", fun = "bagger"),
      defaults = list(base_model = "MARS")
    )
  )

  parsnip::set_encoding(
    model = "bag_mars",
    eng = "earth",
    mode = "regression",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "bag_mars",
    eng = "earth",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "baguette", fun = "bagger"),
      defaults = list(base_model = "MARS")
    )
  )

  parsnip::set_encoding(
    model = "bag_mars",
    eng = "earth",
    mode = "classification",
    options = list(
      predictor_indicators = "none",
      compute_intercept = FALSE,
      remove_intercept = FALSE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "bag_mars",
    eng = "earth",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), new_data = quote(new_data))
    )
  )

  parsnip::set_pred(
    model = "bag_mars",
    eng = "earth",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = fix_column_names,
      func = c(pkg = NULL, fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          type = "class"
        )
    )
  )

  parsnip::set_pred(
    model = "bag_mars",
    eng = "earth",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = fix_column_names,
      func = c(pkg = NULL, fun = "predict"),
      args = list(object = quote(object$fit), new_data = quote(new_data), type = "prob")
    )
  )
}

# nocov end
