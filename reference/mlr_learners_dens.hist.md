# Histogram Density Estimator

Calls [`graphics::hist()`](https://rdrr.io/r/graphics/hist.html) and the
result is coerced to a
[distr6::Distribution](https://xoopr.github.io/distr6/reference/Distribution.html).

## Dictionary

This [Learner](https://mlr3.mlr-org.com/reference/Learner.html) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_learners](https://mlr3.mlr-org.com/reference/mlr_learners.html) or
with the associated sugar function
[lrn()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    LearnerDensHistogram$new()
    mlr_learners$get("dens.hist")
    lrn("dens.hist")

## Meta Information

- Type: "dens"

- Predict Types: `pdf, cdf, distr`

- Feature Types: `integer, numeric`

- Properties: `-`

- Packages: [mlr3](https://CRAN.R-project.org/package=mlr3)
  [mlr3proba](https://CRAN.R-project.org/package=mlr3proba)
  [distr6](https://CRAN.R-project.org/package=distr6)

## See also

Other density estimators:
[`mlr_learners_dens.kde`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.kde.md),
[`mlr_learners_dens.kde_ks`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.kde_ks.md),
[`mlr_learners_dens.locfit`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.locfit.md),
[`mlr_learners_dens.logspline`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.logspline.md),
[`mlr_learners_dens.mixed`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.mixed.md),
[`mlr_learners_dens.nonpar`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.nonpar.md),
[`mlr_learners_dens.pen`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.pen.md),
[`mlr_learners_dens.plug`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.plug.md),
[`mlr_learners_dens.spline`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.spline.md)

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`mlr3proba::LearnerDens`](https://mlr3proba.mlr-org.com/reference/LearnerDens.md)
-\> `LearnerDensHistogram`

## Methods

### Public methods

- [`LearnerDensHistogram$new()`](#method-LearnerDensHistogram-new)

- [`LearnerDensHistogram$clone()`](#method-LearnerDensHistogram-clone)

Inherited methods

- [`mlr3::Learner$base_learner()`](https://mlr3.mlr-org.com/reference/Learner.html#method-base_learner)
- [`mlr3::Learner$configure()`](https://mlr3.mlr-org.com/reference/Learner.html#method-configure)
- [`mlr3::Learner$encapsulate()`](https://mlr3.mlr-org.com/reference/Learner.html#method-encapsulate)
- [`mlr3::Learner$format()`](https://mlr3.mlr-org.com/reference/Learner.html#method-format)
- [`mlr3::Learner$help()`](https://mlr3.mlr-org.com/reference/Learner.html#method-help)
- [`mlr3::Learner$predict()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict)
- [`mlr3::Learner$predict_newdata()`](https://mlr3.mlr-org.com/reference/Learner.html#method-predict_newdata)
- [`mlr3::Learner$print()`](https://mlr3.mlr-org.com/reference/Learner.html#method-print)
- [`mlr3::Learner$reset()`](https://mlr3.mlr-org.com/reference/Learner.html#method-reset)
- [`mlr3::Learner$selected_features()`](https://mlr3.mlr-org.com/reference/Learner.html#method-selected_features)
- [`mlr3::Learner$train()`](https://mlr3.mlr-org.com/reference/Learner.html#method-train)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://rdrr.io/pkg/R6/man/R6Class.html) class.

#### Usage

    LearnerDensHistogram$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerDensHistogram$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Define the Learner
learner = lrn("dens.hist")
print(learner)
#> 
#> ── <LearnerDensHistogram> (dens.hist): Histogram Density Estimator ─────────────
#> • Model: -
#> • Parameters: list()
#> • Packages: mlr3, mlr3proba, and distr6
#> • Predict Types: [pdf], cdf, and distr
#> • Feature Types: integer and numeric
#> • Encapsulation: none (fallback: -)
#> • Properties:
#> • Other settings: use_weights = 'error'

# Define a Task
task = tsk("faithful")

# Create train and test set
ids = partition(task)

# Train the learner on the training ids
learner$train(task, row_ids = ids$train)

print(learner$model)
#> $distr
#> Histogram() 
#> 
#> $hist
#> $breaks
#> [1] 1.5 2.0 2.5 3.0 3.5 4.0 4.5 5.0 5.5
#> 
#> $counts
#> [1] 37 31  1  7 18 53 32  3
#> 
#> $density
#> [1] 0.40659341 0.34065934 0.01098901 0.07692308 0.19780220 0.58241758 0.35164835
#> [8] 0.03296703
#> 
#> $mids
#> [1] 1.75 2.25 2.75 3.25 3.75 4.25 4.75 5.25
#> 
#> $xname
#> [1] "dat"
#> 
#> $equidist
#> [1] TRUE
#> 
#> attr(,"class")
#> [1] "histogram"
#> 
#> attr(,"class")
#> [1] "dens.hist"

# Make predictions for the test rows
predictions = learner$predict(task, row_ids = ids$test)

# Score the predictions
predictions$score()
#> dens.logloss 
#>     1.170993 
```
