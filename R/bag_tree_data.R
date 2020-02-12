# These functions are tested indirectly when the models are used. Since this
# function is executed on package startup, you can't execute them to test since
# they are already in the parsnip model database. We'll exclude them from
# coverage stats for this reason.

# nocov

make_bag_tree <- function() {
  set_new_model("bag_tree")

  set_model_mode("bag_tree", "classification")
  set_model_mode("bag_tree", "regression")

  # ----------------------------------------------------------------------------

  set_model_engine("bag_tree", "classification", "rpart")
  set_model_engine("bag_tree", "regression", "rpart")
  set_dependency("bag_tree", "rpart", "rpart")

  set_model_arg(
    model = "bag_tree",
    eng = "rpart",
    parsnip = "tree_depth",
    original = "maxdepth",
    func = list(pkg = "dials", fun = "tree_depth"),
    has_submodel = FALSE
  )

  set_model_arg(
    model = "bag_tree",
    eng = "rpart",
    parsnip = "min_n",
    original = "minsplit",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  set_model_arg(
    model = "bag_tree",
    eng = "rpart",
    parsnip = "cost_complexity",
    original = "cp",
    func = list(pkg = "dials", fun = "cost_complexity"),
    has_submodel = FALSE
  )

  set_fit(
    model = "bag_tree",
    eng = "rpart",
    mode = "regression",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "rpart", fun = "rpart"),
      defaults = list()
    )
  )

  set_fit(
    model = "bag_tree",
    eng = "rpart",
    mode = "classification",
    value = list(
      interface = "formula",
      protect = c("formula", "data", "weights"),
      func = c(pkg = "rpart", fun = "rpart"),
      defaults = list()
    )
  )

  set_pred(
    model = "bag_tree",
    eng = "rpart",
    mode = "regression",
    type = "numeric",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )

  set_pred(
    model = "bag_tree",
    eng = "rpart",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = NULL, fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "class"
        )
    )
  )

  set_pred(
    model = "bag_tree",
    eng = "rpart",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(pkg = NULL, fun = "predict"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )

  # ------------------------------------------------------------------------------

  set_model_engine("bag_tree", "classification", "C5.0")
  set_dependency("bag_tree", "C5.0", "C50")

  set_model_arg(
    model = "bag_tree",
    eng = "C5.0",
    parsnip = "min_n",
    original = "minCases",
    func = list(pkg = "dials", fun = "min_n"),
    has_submodel = FALSE
  )

  set_fit(
    model = "bag_tree",
    eng = "C5.0",
    mode = "classification",
    value = list(
      interface = "data.frame",
      protect = c("x", "y", "weights"),
      func = c(pkg = "parsnip", fun = "C5.0_train"),
      defaults = list(trials = 1)
    )
  )

  set_pred(
    model = "bag_tree",
    eng = "C5.0",
    mode = "classification",
    type = "class",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args = list(object = quote(object$fit), newdata = quote(new_data))
    )
  )

  set_pred(
    model = "bag_tree",
    eng = "C5.0",
    mode = "classification",
    type = "prob",
    value = list(
      pre = NULL,
      post = NULL,
      func = c(fun = "predict"),
      args =
        list(
          object = quote(object$fit),
          newdata = quote(new_data),
          type = "prob"
        )
    )
  )

}

# nocov end
