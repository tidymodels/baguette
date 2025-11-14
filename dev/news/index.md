# Changelog

## baguette (development version)

- Transition from the magrittr pipe to the vase R pipe.

## baguette 1.1.0

CRAN release: 2025-01-28

- Small update with improved error and warning logging.

## baguette 1.0.2

CRAN release: 2024-02-13

- Move the earth package to Suggests due to CRAN note about the
  TeachingDemos
  ([\#71](https://github.com/tidymodels/baguette/issues/71)).

## baguette 1.0.1

CRAN release: 2023-04-04

- Added a `"nnet"` method for
  [`bagger()`](https://baguette.tidymodels.org/dev/reference/bagger.md).

## baguette 1.0.0

CRAN release: 2022-06-17

- Enable case weights for
  [`bagger()`](https://baguette.tidymodels.org/dev/reference/bagger.md).

## baguette 0.2.0

CRAN release: 2022-03-09

- Model definitions were moved to parsnip
  ([\#47](https://github.com/tidymodels/baguette/issues/47))

## baguette 0.1.1

CRAN release: 2021-07-14

- Bug fix for cases where CART cannot find a split
  ([\#37](https://github.com/tidymodels/baguette/issues/37))

- Updated internal usage of a soft-deprecated argument to
  [`hardhat::default_formula_blueprint()`](https://hardhat.tidymodels.org/reference/default_formula_blueprint.html).

## baguette 0.1.0

CRAN release: 2020-10-28

- Added encoding information to work with current version of `parsnip`.

## baguette 0.0.0.9000

Development version
