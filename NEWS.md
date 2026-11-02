# baguette (development version)

* Transition from the magrittr pipe to the vase R pipe.

* The `C5.0` engine for `bag_tree()` now registers `CF`, `noGlobalPruning`, `winnow`, and `fuzzyThreshold` as tunable engine arguments, and the `earth` engine for `bag_mars()` now registers `nk`, so that they map to their corresponding dials parameters.

# baguette 1.1.0

* Small update with improved error and warning logging. 

# baguette 1.0.2

* Move the earth package to Suggests due to CRAN note about the TeachingDemos (#71).

# baguette 1.0.1

* Added a `"nnet"` method for `bagger()`. 

# baguette 1.0.0

* Enable case weights for `bagger()`. 

# baguette 0.2.0

* Model definitions were moved to parsnip (#47)

# baguette 0.1.1

* Bug fix for cases where CART cannot find a split (#37)

* Updated internal usage of a soft-deprecated argument to `hardhat::default_formula_blueprint()`.

# baguette 0.1.0

* Added encoding information to work with current version of `parsnip`. 

# baguette 0.0.0.9000

Development version

