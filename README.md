
<!-- README.md is generated from README.Rmd. Please edit that file -->

# baguette <a href="https://baguette.tidymodels.org/"><img src="man/figures/logo.png" align="right" height="138" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidymodels/baguette/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidymodels/baguette/actions/workflows/R-CMD-check.yaml)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
status](https://www.r-pkg.org/badges/version/baguette)](https://cran.r-project.org/package=baguette)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/baguette/branch/main/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/baguette?branch=main)
<!-- badges: end -->

## Introduction

The goal of baguette is to provide efficient functions for bagging (aka
[bootstrap
aggregating](https://scholar.google.com/scholar?hl=en&as_sdt=0%2C7&q=bagging+predictors+breiman+1996&oq=Bagging+predictors+))
ensemble models.

The model objects produced by baguette are kept smaller than they would
otherwise be through two operations:

- The [butcher](https://butcher.tidymodels.org/) package is used to
  remove object elements that are not crucial to using the models. For
  example, some models contain copies of the training set or model
  residuals when created. These are removed to save space.

- For ensembles whose base models use a formula method, there is a
  built-in redundancy because each model has an identical terms object.
  However, each one of these takes up separate space in memory and can
  be quite large when there are many predictors. The baguette package
  solves this problem by replacing each terms object with the object
  from the first model in the ensemble. Since the other terms objects
  are not modified, we get the same functional capabilities using far
  less memory to save the ensemble.

## Installation

You can install the released version of baguette from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("baguette")
```

Install the development version from GitHub with:

``` r
require("devtools")
install_github("tidymodels/baguette")
```

## Available Engines

The baguette package provides engines for the models in the following
table.

| model    | engine | mode           |
|:---------|:-------|:---------------|
| bag_mars | earth  | classification |
| bag_mars | earth  | regression     |
| bag_mlp  | nnet   | classification |
| bag_mlp  | nnet   | regression     |
| bag_tree | rpart  | classification |
| bag_tree | rpart  | regression     |
| bag_tree | C5.0   | classification |

## Example

Let’s build a bagged decision tree model to predict a continuous
outcome.

``` r
library(baguette)

bag_tree() %>% 
  set_engine("rpart") # C5.0 is also available here
#> Bagged Decision Tree Model Specification (unknown mode)
#> 
#> Main Arguments:
#>   cost_complexity = 0
#>   min_n = 2
#> 
#> Computational engine: rpart

set.seed(123)
bag_cars <- 
  bag_tree() %>% 
  set_engine("rpart", times = 25) %>% # 25 ensemble members 
  set_mode("regression") %>% 
  fit(mpg ~ ., data = mtcars)

bag_cars
#> parsnip model object
#> 
#> Bagged CART (regression with 25 members)
#> 
#> Variable importance scores include:
#> 
#> # A tibble: 10 × 4
#>    term  value std.error  used
#>    <chr> <dbl>     <dbl> <int>
#>  1 disp  905.       51.9    25
#>  2 wt    889.       56.8    25
#>  3 hp    814.       48.7    25
#>  4 cyl   581.       42.9    25
#>  5 drat  540.       54.1    25
#>  6 qsec  281.       53.2    25
#>  7 vs    150.       51.2    20
#>  8 carb   84.4      30.6    25
#>  9 gear   80.0      35.8    23
#> 10 am     51.5      22.9    18
```

The models also return aggregated variable importance scores.

## Contributing

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

- For questions and discussions about tidymodels packages, modeling, and
  machine learning, please [post on Posit
  Community](https://community.rstudio.com/new-topic?category_id=15&tags=tidymodels,question).

- If you think you have encountered a bug, please [submit an
  issue](https://github.com/tidymodels/baguette/issues).

- Either way, learn how to create and share a
  [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
  (a minimal, reproducible example), to clearly communicate about your
  code.

- Check out further details on [contributing guidelines for tidymodels
  packages](https://www.tidymodels.org/contribute/) and [how to get
  help](https://www.tidymodels.org/help/).
