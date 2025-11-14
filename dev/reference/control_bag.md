# Controlling the bagging process

`control_bag()` can set options for ancillary aspects of the bagging
process.

## Usage

``` r
control_bag(
  var_imp = TRUE,
  allow_parallel = TRUE,
  sampling = "none",
  reduce = TRUE,
  extract = NULL
)
```

## Arguments

- var_imp:

  A single logical: should variable importance scores be calculated?

- allow_parallel:

  A single logical: should the model fits be done in parallel (even if a
  parallel `plan()` has been created)?

- sampling:

  Either "none" or "down". For classification only. The training data,
  after bootstrapping, will be sampled down within each class (with
  replacement) to the size of the smallest class.

- reduce:

  Should models be modified to reduce their size on disk?

- extract:

  A function (or NULL) that can extract model-related aspects of each
  ensemble member. See Details and example below.

## Value

A list.

## Details

Any arbitrary item can be saved from the model object (including the
model object itself) using the `extract` argument, which should be a
function with arguments `x` (for the model object), and `...`. The
results of this function are saved into a list column called `extras`
(see the example below).

## Examples

``` r
# Extracting model components

num_term_nodes <- function(x, ...) {
  tibble::tibble(num_nodes = sum(x$frame$var == "<leaf>"))
}

set.seed(7687)
with_extras <- bagger(mpg ~ ., data = mtcars,
                      base_model = "CART", times = 5,
                      control = control_bag(extract = num_term_nodes))

dplyr::bind_rows(with_extras$model_df$extras)
#> # A tibble: 5 × 1
#>   num_nodes
#>       <int>
#> 1         3
#> 2         3
#> 3         3
#> 4         3
#> 5         2
```
