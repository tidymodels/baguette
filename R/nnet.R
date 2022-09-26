
nnet_bagger <- function(rs, control, ...) {
  opt <- rlang::dots_list(...)
  is_classif <- is.factor(rs$splits[[1]]$data$.outcome)
  mod_spec <- make_nnet_spec(is_classif, opt)

  iter <- get_iterator(control)

  rs <-
    rs %>%
    dplyr::mutate(
      model = iter(
        fit_seed,
        splits,
        seed_fit,
        .fn = nnet_fit,
        spec = mod_spec,
        control = control
      )
    )

  rs <- check_for_disaster(rs)

  rs <- filter_rs(rs)

  rs <- extractor(rs, control$extract)

  imps <- compute_imp(rs, nnet_imp_garson, control$var_imp)

  rs <-
    rs %>%
    replace_parsnip_terms()

  if (control$reduce) {
    rs <-
      rs %>%
      mutate(model = map(model, axe_nnet))
  }

  list(model = rs, imp = imps)
}

make_nnet_spec <- function(classif, opt) {
  opts <- join_args(model_defaults[["nnet"]], opt)
  if (classif) {
    nnet_md <- "classification"
  } else {
    nnet_md <- "regression"
  }
  nnet_spec <-
    parsnip::mlp(
      mode = nnet_md,
      penalty = !!opts$decay,
      hidden_units = !!opts$size,
      epochs = !!opt$maxit
    )

  opts <- opts[!(names(opts) %in% c("decay", "size", "maxit"))]

  if (length(opts) > 0) {
    main_args <- list()

    if (length(opts) == 0) {
      opts <- NULL
    }
    if (length(main_args) == 0) {
      main_args <- NULL
    }
    nnet_spec <-
      parsnip::set_engine(nnet_spec, engine = "nnet", !!!main_args, !!!opts)
  } else {
    nnet_spec <- parsnip::set_engine(nnet_spec, engine = "nnet")
  }

  nnet_spec
}


nnet_fit <- function(split, spec, control = control_bag()) {

  dat <- rsample::analysis(split)

  if (control$sampling == "down") {
    dat <- down_sampler(dat)
  }
  if (any(names(dat) == ".weights")) {
    wts <- hardhat::importance_weights(dat$.weights)
    dat$.weights <- NULL
  } else {
    wts <- NULL
  }

  ctrl <- parsnip::control_parsnip(catch = TRUE)
  mod <-
    parsnip::fit.model_spec(
      spec,
      .outcome ~ .,
      data = dat,
      control = ctrl,
      case_weights = wts
    )
  mod
}

axe_nnet <- function(x) {
  x$fit <- butcher::axe_fitted(x$fit)
  x$fit <- butcher::axe_call(x$fit)
  x$fit <- butcher::axe_env(x$fit)
  x
}


#' Garson Importance Scores for neural Networks
#' @param object A `model_fit` or `nnet` object
#' @return A tibble.
#' @export
#' @keywords internal
nnet_imp_garson <- function(object) {
  if (inherits(object, "model_fit")) {
    object <- object$fit
  }
  beta <- coef(object)
  abeta <- abs(beta)
  nms <- names(beta)
  i2h <- array(NA, dim = object$n[2:1])
  h2o <- array(NA, dim = object$n[2:3])

  for (hidden in 1:object$n[2]) {
    for (input in 1:object$n[1]) {
      label <- paste("i", input, "->h", hidden, "$", sep = "")
      i2h[hidden, input] <- abeta[grep(label, nms, fixed = FALSE)]
    }
  }
  for(hidden in 1:object$n[2]) {
    for(output in 1:object$n[3]) {
      label <- paste("h", hidden, "->o",
                     ifelse(object$n[3] == 1, "", output),
                     sep = "")
      h2o[hidden, output] <- abeta[grep(label, nms, fixed = TRUE)]
    }
  }

  if(FALSE) {
    ## Test case from Gevrey, M., Dimopoulos, I., & Lek,
    ## S. (2003). Review and comparison of methods to study the
    ## contribution of variables in artificial neural network
    ## models. ecological modelling, 160(3), 249-264.
    i2h <- matrix(c(-1.67624,  3.29022,  1.32466,
                    -0.51874, -0.22921, -0.25526,
                    -4.01764,  2.12486, -0.08168,
                    -1.75691, -1.44702,  0.58286),
                  ncol = 3, byrow = TRUE)
    h2o <- matrix(c(4.57857, -0.48815, -5.73901, -2.65221),
                  ncol = 1)
  }

  ##  From Gevrey et al. (2003): "For each hidden neuron i, multiply
  ##  the absolute value of the hidden-output layer connection
  ##  weight by the absolute value of the hidden-input layer
  ##  connection weight. Do this for each input variable j. The
  ##  following products Pij are obtained"


  ## We'll do this one response at a time. Gevrey et al. (2003) do
  ## not discuss multiple outputs, but the results are the same (at
  ## least in the case of classification).

  imp <- matrix(NA, nrow = object$n[1], ncol = object$n[3])


  for (output in 1:object$n[3]) {
    Pij <- i2h * NA
    for (hidden in 1:object$n[2]) Pij[hidden, ] <- i2h[hidden, ] * h2o[hidden, output]

    ## "For each hidden neuron, divide Pij by the sum for all the
    ## input variables to obtain Qij. For example for Hidden 1, Q11 =
    ## P11/(P11+P12+P13).

    Qij <- Pij * NA
    for (hidden in 1:object$n[2]) Qij[hidden, ] <- Pij[hidden, ] / sum(Pij[hidden, ])


    ## "For each input neuron, sum the product Sj formed from the
    ## previous computations of Qij. For example, S1 =
    ## Q11+Q21+Q31+Q41."

    Sj <- apply(Qij, 2, sum)

    ## "Divide Sj by the sum for all the input variables. Expressed as
    ## a percentage, this gives the relative importance or
    ## distribution of all output weights attributable to the given
    ## input variable. For example, for the input neuron 1, the
    ## relative importance is equal to (S1/100)/(S1+S2+S3)"

    imp[, output] <- Sj / sum(Sj) * 100
  }

  imp <- apply(imp, 1, mean)

  if (!is.null(object$coefnames)) {
    x_nms <- object$coefnames
  } else {
    ind <- format(1:object$n[1])
    ind <- gsub(" ", "0", ind)
    x_nms <- paste0("predictor_", ind)
  }

  tibble::tibble(predictor = x_nms, importance = imp)
}
