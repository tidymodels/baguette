library(nnet)
library(NeuralNetTools)
library(baguette)
library(datapasta)
library(tibble)

# ------------------------------------------------------------------------------
# reformat output from NeuralNetTools
nnt_reformat <- function(x) {
  x <- tibble::tibble(predictor = rownames(x), importance = x[,1] * 100)
  datapasta::tribble_paste(x)
}

# ------------------------------------------------------------------------------

set.seed(1)
reg_mod <- nnet(mpg ~ ., data = mtcars, size = 3, trace = FALSE)

baguette:::nnet_imp_garson(reg_mod)
NeuralNetTools::garson(reg_mod, bar_plot = FALSE) %>% nnt_reformat()
