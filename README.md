---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->


# baguette

<!-- badges: start -->
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN status](https://www.r-pkg.org/badges/version/baguette)](https://cran.r-project.org/package=baguette)
<!-- badges: end -->

The goal of baguette is to provide efficient functions that can be used to create ensemble models. 

Currently in development/draft. Major to-do's are:

 * Convert model objects to expressions _a la_ `tidypredict`
  
 * To compute out-of-bag performance, pass in a `yardstick` `metric_set` object

 * Test out `hardhat` more

 * Add tree-based models for two-class models that have asymmetric costs for each class 


## Installation

For now:

``` r
devtools::install_github("topepo/baguette")
```

## Example

