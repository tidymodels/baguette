#' @importFrom rpart rpart
#' @importFrom rsample analysis
#' @importFrom purrr map map2 map_df
#' @importFrom tibble tibble
#' @importFrom parsnip decision_tree
#' @importFrom furrr future_map
#' @importFrom partykit as.party.rpart

cart_bagger <- function(rs, opt, var_imp, oob, extract, ...) {
  is_classif <- is.factor(rs$splits[[1]]$data$.outcome)
  mod_spec <- make_cart_spec(is_classif, opt)

  rs <-
    rs %>%
    dplyr::mutate(model = furrr::future_map(splits, cart_fit, spec = mod_spec))

  rs <- check_for_disaster(rs)

  rs <- filter_rs(rs)

  rs <- extractor(rs, extract)

  imps <- compute_imp(rs, cart_imp, var_imp)

  oob <- compute_oob(rs, oob)

  rs <-
    rs %>%
    mutate(
      model = map(model, ~ partykit::as.party.rpart(.x$fit)),
      .pred_form = map(model, tidypredict::tidypredict_fit)
    )

  list(model = select_rs(rs), oob  = oob, imp = imps)
}

make_cart_spec <- function(classif, opt) {
  opts <- join_args(model_defaults[["CART"]], opt)
  if (classif) {
    cart_md <- "classification"
  } else {
    cart_md <- "regression"
  }
  cart_spec <-
    parsnip::decision_tree(
      mode = cart_md,
      cost_complexity = !!opts$cp,
      min_n = !!opts$minsplit,
      tree_depth = !!opts$maxdepth
    )

  opts <- opts[!(names(opts) %in% c("cp", "maxdepth", "minsplit"))]

  if (length(opts) > 0) {
    main_args <- list()

    if (any(names(opts) == "method")) {
      main_args$method <- opts$method
      opts$method <- NULL
    }
    if (any(names(opts) == "parms")) {
      main_args$parms <- opts$parms
      opts$parms <- NULL
    }
    if (any(names(opts) == "cost")) {
      main_args$cost <- opts$cost
      opts$cost <- NULL
    }
    if (length(opts) == 0) {
      opts <- NULL
    }
    if (length(main_args) == 0) {
      main_args <- NULL
    }

    # Note: from ?rpart: "arguments to rpart.control may also be specified in
    # the call to rpart. They are checked against the list of valid arguments."

    cart_spec <-
      parsnip::set_engine(cart_spec, engine = "rpart", !!!main_args, !!!opts)
  } else {
    cart_spec <- parsnip::set_engine(cart_spec, engine = "rpart")
  }
  cart_spec
}

#' @importFrom stats complete.cases

cart_fit  <- function(split, spec) {
  ctrl <- parsnip::fit_control(catch = TRUE)
  mod <-
    parsnip::fit.model_spec(spec,
                            .outcome ~ .,
                            data = rsample::analysis(split),
                            control = ctrl)
  mod
}

cart_imp <- function(x) {
  x <-
    tibble::tibble(
      predictor = names(x$fit$variable.importance),
      importance = unname(x$fit$variable.importance)
    )
  x <- x[x$importance > 0,]
  x
}


