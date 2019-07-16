#' @importFrom Cubist cubist cubistControl
#' @importFrom rsample analysis
#' @importFrom purrr map_lgl map2 map_df
#' @importFrom tibble as_tibble
#' @importFrom furrr future_map2

cubist_bagger <- function(rs, opt, var_imp, oob, extract, ...) {

  rs <-
    rs %>%
    dplyr::mutate(model = furrr::future_map2(splits, fit_seed, cubist_fit, opt = opt))

  rs <- check_for_disaster(rs)

  rs <- filter_rs(rs)

  rs <- extractor(rs, extract)

  imps <- compute_imp(rs, cubist_imp, var_imp)

  oob <- compute_oob(rs, oob)

  rs <- rs %>% mutate(.pred_form = map(model, tidypredict::tidypredict_fit))

  list(model = select_rs(rs), oob  = oob, imp = imps)
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


