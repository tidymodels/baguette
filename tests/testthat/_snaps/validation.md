# bad values

    Code
      baguette:::validate_args(model = "mars", times = 5L, control = control_bag(),
      cost = NULL)
    Condition
      Error:
      ! `base_model` must be one of "CART", "C5.0", "MARS", or "nnet", not "mars".
      i Did you mean "MARS"?

---

    Code
      baguette:::validate_args(model = "MARS", times = 1, control = control_bag(),
      cost = NULL)
    Condition
      Error:
      ! `times` must be a whole number larger than or equal to 2, not the number 1.

---

    Code
      baguette:::validate_args(model = "MARS", times = -1L, control = control_bag(),
      cost = NULL)
    Condition
      Error:
      ! `times` must be a whole number larger than or equal to 2, not the number -1.

---

    Code
      baguette:::validate_args(model = "MARS", times = 5L, control = 2, cost = NULL)
    Condition
      Error:
      ! The control object should be a list created by `control_bag()`, not the number 2.

---

    Code
      bagger(Sepal.Length ~ ., data = iris, times = 2L, base_model = "CART", cost = 2)
    Condition
      Error in `bagger()`:
      ! `cost` can only be a scalar when there are two levels.

# wrong y for C5

    Code
      bagger(Sepal.Length ~ ., data = iris, times = 2L, base_model = "C5.0")
    Condition
      Error in `validate_outcomes_are_factors()`:
      ! All outcomes must be factors.
      i The following is not:
      "Sepal.Length": <numeric>

# validate imps

    Code
      baguette:::validate_importance(tibble::tibble(terms = letters[1:2], value = 1:2,
      std.error = 1:2))
    Condition
      Error:
      ! Importance columns should be: "term", "value", "std.error", and "used".

---

    Code
      baguette:::validate_importance(data.frame(term = letters[1:2], value = 1:2,
      std.error = 1:2))
    Condition
      Error:
      ! Imprtance score results should be a tibble, not a <data.frame> object.

# bad inputs

    Code
      bagger(mpg ~ ., data = mtcars, base_model = letters[1:2])
    Condition
      Error in `bagger()`:
      ! `base_model` must be one of "CART", "C5.0", "MARS", or "nnet", not "a".

---

    Code
      bagger(mpg ~ ., data = mtcars, base_model = "MARS", cost = 2)
    Condition
      Error in `bagger()`:
      ! When using misclassification costs, `base_model` should be either "CART" or "C5.0", not the string "MARS".

---

    Code
      bagger(mpg ~ ., data = mtcars, base_model = "CART", cost = -2)
    Condition
      Error in `bagger()`:
      ! `cost` must be a number larger than or equal to 0 or `NULL`, not the number -2.

---

    Code
      bagger(mpg ~ ., data = mtcars, base_model = "CART", cost = matrix(1, ncol = 2,
        nrow = 1))
    Condition
      Error in `bagger()`:
      ! If `cost` is a matrix, is must be numeric and square.

---

    Code
      bagger(mpg ~ ., data = mtcars, base_model = "MARS", control = control_bag(
        extract = 2))
    Condition
      Error in `control_bag()`:
      ! `extract` must be a function or `NULL`, not the number 2.

---

    Code
      bagger(mpg ~ ., data = mtcars, base_model = "C5.0")
    Condition
      Error in `validate_outcomes_are_factors()`:
      ! All outcomes must be factors.
      i The following is not:
      "mpg": <numeric>

---

    Code
      bagger(wt + mpg ~ ., data = mtcars, base_model = "MARS")
    Condition
      Error in `validate_outcomes_are_univariate()`:
      ! The outcome must be univariate, but 2 columns were found.

---

    Code
      predict(bagger(mpg ~ ., data = mtcars, base_model = "MARS"), mtcars[1:2, -1],
      type = "potato")
    Condition
      Error in `predict()`:
      ! `type` must be one of "numeric", not "potato".

---

    Code
      set.seed(3983)
      predict(bagger(Class ~ ., data = two_class_dat, base_model = "MARS"),
      two_class_dat[1:2, -3], type = "topepo")
    Condition
      Warning:
      There were 2 warnings in `dplyr::mutate()`.
      The first warning was:
      i In argument: `model = iter(...)`.
      Caused by warning:
      ! glm.fit: fitted probabilities numerically 0 or 1 occurred
      i Run `dplyr::last_dplyr_warnings()` to see the 1 remaining warning.
      Error in `predict()`:
      ! `type` must be one of "class" or "prob", not "topepo".

# model failures inputs

    Code
      set.seed(459394)
      bagger(a ~ ., data = bad_iris, base_model = "CART", times = 3)
    Condition
      Error in `bagger()`:
      ! All of the models failed. Example:
      x Error in cbind(yval2, yprob, nodeprob) : number of rows of matrices must match (see arg 2)

# control inputs

    Code
      control_bag(reduce = 1:2)
    Condition
      Error in `control_bag()`:
      ! `reduce` must be `TRUE` or `FALSE`, not an integer vector.

---

    Code
      control_bag(reduce = 1)
    Condition
      Error in `control_bag()`:
      ! `reduce` must be `TRUE` or `FALSE`, not the number 1.

---

    Code
      control_bag(sampling = rep("none", 2))
    Condition
      Error in `control_bag()`:
      ! `arg` must be length 1 or a permutation of `c("none", "down")`.

---

    Code
      control_bag(sampling = 1)
    Condition
      Error in `control_bag()`:
      ! `sampling` must be a string or character vector.

---

    Code
      control_bag(allow_parallel = 1:2)
    Condition
      Error in `control_bag()`:
      ! `allow_parallel` must be `TRUE` or `FALSE`, not an integer vector.

---

    Code
      control_bag(allow_parallel = 1)
    Condition
      Error in `control_bag()`:
      ! `allow_parallel` must be `TRUE` or `FALSE`, not the number 1.

---

    Code
      control_bag(var_imp = 1:2)
    Condition
      Error in `control_bag()`:
      ! `var_imp` must be `TRUE` or `FALSE`, not an integer vector.

---

    Code
      control_bag(var_imp = 1)
    Condition
      Error in `control_bag()`:
      ! `var_imp` must be `TRUE` or `FALSE`, not the number 1.

