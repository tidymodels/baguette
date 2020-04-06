# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_bag_tree <- function() {
  parsnip::set_new_model("bag_tree")

  parsnip::set_model_mode("bag_tree", "classification")
  parsnip::set_model_mode("bag_tree", "regression")

  # ----------------------------------------------------------------------------

  parsnip::set_model_engine("bag_tree", "classification", "rpart")
  parsnip::set_model_engine("bag_tree", "regression", "rpart")
  parsnip::set_dependency("bag_tree", "rpart", "rpart")

  parsnip::set_model_arg(
    model = "bag_tree",
    eng = "rpart",
    parsnip = "class_cost",
    original = "cost",
    func = list(pkg = "baguette", fun = "class_cost"),
    has_submodel = FALSE
  )
  parsnip::set_model_arg(
    model = "bag_tree",
    eng = "rpart",
    parsnip = "tree_depth",
    original = "maxdepth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "bag_tree",
    eng = "rpart",
    parsnip = "min_n",
    original = "minsplit",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "bag_tree",
    eng = "rpart",
    parsnip = "cost_complexity",
    original = "cp",
    func = list(pkg = "dials", fun = "cost_complexity"),
    has_submodel = FALSE
  )

  parsnip::set_fit(
    model = "bag_tree",
    eng = "rpart",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "baguette", fun = "bagger"),
      defaults = list(base_model = "CART")
    )
  )

  parsnip::set_fit(
    model = "bag_tree",
    eng = "rpart",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "baguette", fun = "bagger"),
      defaults = list(base_model = "CART")
    )
  )

  parsnip::set_pred(
    model = "bag_tree",
    eng = "rpart",
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
    model = "bag_tree",
    eng = "rpart",
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
    model = "bag_tree",
    eng = "rpart",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = fix_column_names,
      func = c(pkg = NULL, fun = "predict"),
      args = list(object = quote(object$fit), new_data = quote(new_data), type = "prob")
    )
  )

  # ------------------------------------------------------------------------------

  parsnip::set_model_engine("bag_tree", "classification", "C5.0")
  parsnip::set_dependency("bag_tree", "C5.0", "C50")

  parsnip::set_fit(
    model = "bag_tree",
    eng = "C5.0",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "baguette", fun = "bagger"),
      defaults = list(base_model = "C5.0")
    )
  )


  parsnip::set_model_arg(
    model = "bag_tree",
    eng = "C5.0",
    parsnip = "class_cost",
    original = "cost",
    func = list(pkg = "baguette", fun = "class_cost"),
    has_submodel = FALSE
  )

  parsnip::set_model_arg(
    model = "bag_tree",
    eng = "C5.0",
    parsnip = "min_n",
    original = "minCases",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  parsnip::set_pred(
    model = "bag_tree",
    eng = "C5.0",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(
        object = quote(object$fit),
        new_data = quote(new_data),
        type = "class"
      )
    )
  )

  parsnip::set_pred(
    model = "bag_tree",
    eng = "C5.0",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = fix_column_names,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          new_data = quote(new_data),
          type = "prob"
        )
    )
  )

}

# nocov end
