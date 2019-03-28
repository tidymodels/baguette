#' @importFrom Cubist cubist cubistControl
#' @importFrom rsample analysis
#' @importFrom purrr map_lgl map2 map_df
#' @importFrom tibble as_tibble

cubist_bagger <- function(rs, opt, var_imp, oob, extract, ...) {
  rs <-
    rs %>%
    dplyr::mutate(
      model = purrr::map2(splits, fit_seed, cubist_fit, opt = opt),
      passed = !purrr::map_lgl(model, model_failure)
    )

  check_for_disaster(rs)

  rs <- rs %>% dplyr::filter(passed)
  num_mod <- nrow(rs)

  if (var_imp) {
    imps <-
      purrr::map_df(rs$model, cubist_imp) %>%
      dplyr::group_by(predictor) %>%
      dplyr::summarize(
        importance = sum(importance)/num_mod,
        used = length(predictor)
      ) %>%
      dplyr::arrange(desc(importance))
  } else {
    imps <- NULL
  }

  if (!is.null(extract)) {
    rs <-
      rs %>%
      dplyr::mutate(extras = map(model, extract, ...))
  }

  list(model = rs %>% dplyr::select(-splits, -id, -fit_seed, -passed), imp = imps)
}

cubist_control <- function(opt, seed) {
  cb_call <- rlang::call2("cubistControl", !!!opt, seed = seed, .ns = "Cubist")
  rlang::eval_tidy(cb_call)
}

cubist_fit  <- function(split, seed, opt) {
  ctrl <- cubist_control(opt, seed)
  dat <- rsample::analysis(split)
  mod <-
    try(
      Cubist::cubist(
        x = dat[, names(dat) != ".outcome", drop = FALSE],
        y = dat$.outcome,
        control = ctrl,
        committees = 1
      ),
      silent = TRUE)
  mod
}

cubist_imp <- function(x) {
  x <- x$usage
  x$importance <- (x$Conditions + x$Model)/2
  x$predictor <- x$Variable
  x <- x[, c("predictor", "importance")]
  x <- x[x$importance > 0,]
  tibble::as_tibble(x)
}


