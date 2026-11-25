# Cost parameter for minority class

Used in `bag_treer()`.

## Usage

``` r
class_cost(range = c(0, 5), trans = NULL)
```

## Arguments

- range:

  A two-element vector holding the *defaults* for the smallest and
  largest possible values, respectively.

- trans:

  A `trans` object from the `scales` package, such as
  [`scales::log10_trans()`](https://scales.r-lib.org/reference/transform_log.html)
  or
  [`scales::reciprocal_trans()`](https://scales.r-lib.org/reference/transform_reciprocal.html).
  If not provided, the default is used which matches the units used in
  `range`. If no transformation, `NULL`.

## Details

This parameter reflects the cost of a misclassified sample relative to a
baseline cost of 1.0. For example, if the first level of an outcome
factor occurred rarely, it might help if this parameter were set to
values greater than 1.0. If the second level of the outcome factor is in
the minority, values less than 1.0 would cause the model to emphasize
the minority class more than the majority class.

## Examples

``` r
class_cost()
#> Class Cost (quantitative)
#> Range: [0, 5]
```
