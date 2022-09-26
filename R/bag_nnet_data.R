# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_bag_mlp <- function() {

  parsnip::set_model_engine("bag_mlp", "classification", "nnet")
  parsnip::set_model_engine("bag_mlp", "regression", "nnet")
  parsnip::set_dependency("bag_mlp", "nnet", "nnet", mode = "classification")
  parsnip::set_dependency("bag_mlp", "nnet", "nnet", mode = "regression")
  parsnip::set_dependency("bag_mlp", "nnet", "baguette", mode = "classification")
  parsnip::set_dependency("bag_mlp", "nnet", "baguette", mode = "regression")

  parsnip::set_model_arg(
    model = "bag_mlp",
    eng = "nnet",
    parsnip = "hidden_units",
    original = "size",
    func = list(pkg = "dials", fun = "hidden_units"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "bag_mlp",
    eng = "nnet",
    parsnip = "penalty",
    original = "decay",
    func = list(pkg = "dials", fun = "penalty"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "bag_mlp",
    eng = "nnet",
    parsnip = "epochs",
    original = "maxit",
    func = list(pkg = "dials", fun = "epochs"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "bag_mlp",
    eng = "nnet",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "baguette", fun = "bagger"),
      defaults = list(base_model = "nnet")
    )
  )

  parsnip::set_encoding(
    model = "bag_mlp",
    eng = "nnet",
    mode = "regression",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_fit(
    model = "bag_mlp",
    eng = "nnet",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "baguette", fun = "bagger"),
      defaults = list(base_model = "nnet")
    )
  )

  parsnip::set_encoding(
    model = "bag_mlp",
    eng = "nnet",
    mode = "classification",
    options = list(
      predictor_indicators = "traditional",
      compute_intercept = TRUE,
      remove_intercept = TRUE,
      allow_sparse_x = FALSE
    )
  )

  parsnip::set_pred(
    model = "bag_mlp",
    eng = "nnet",
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
    model = "bag_mlp",
    eng = "nnet",
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
    model = "bag_mlp",
    eng = "nnet",
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
