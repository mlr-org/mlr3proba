# Local Density Estimator

Calls
[`locfit::density.lf()`](https://rdrr.io/pkg/locfit/man/density.lf.html)
and the result is coerced to a
[distr6::Distribution](https://xoopr.github.io/distr6/reference/Distribution.html).

## Dictionary

This [Learner](https://mlr3.mlr-org.com/reference/Learner.html) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_learners](https://mlr3.mlr-org.com/reference/mlr_learners.html) or
with the associated sugar function
[lrn()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    LearnerDensLocfit$new()
    mlr_learners$get("dens.locfit")
    lrn("dens.locfit")

## Meta Information

- Type: "dens"

- Predict Types: `pdf`

- Feature Types: `integer, numeric`

- Properties: `-`

- Packages: [mlr3](https://CRAN.R-project.org/package=mlr3)
  [mlr3proba](https://CRAN.R-project.org/package=mlr3proba)
  [locfit](https://CRAN.R-project.org/package=locfit)
  [distr6](https://CRAN.R-project.org/package=distr6)

## References

Loader, Clive (2006). *Local regression and likelihood*. Springer
Science & Business Media.

## See also

Other density estimators:
[`mlr_learners_dens.hist`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.hist.md),
[`mlr_learners_dens.kde`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.kde.md),
[`mlr_learners_dens.kde_ks`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.kde_ks.md),
[`mlr_learners_dens.logspline`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.logspline.md),
[`mlr_learners_dens.mixed`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.mixed.md),
[`mlr_learners_dens.nonpar`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.nonpar.md),
[`mlr_learners_dens.pen`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.pen.md),
[`mlr_learners_dens.plug`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.plug.md),
[`mlr_learners_dens.spline`](https://mlr3proba.mlr-org.com/reference/mlr_learners_dens.spline.md)

## Super classes

[`mlr3::Learner`](https://mlr3.mlr-org.com/reference/Learner.html) -\>
[`mlr3proba::LearnerDens`](https://mlr3proba.mlr-org.com/reference/LearnerDens.md)
-\> `LearnerDensLocfit`

## Methods

### Public methods

- [`LearnerDensLocfit$new()`](#method-LearnerDensLocfit-new)

- [`LearnerDensLocfit$clone()`](#method-LearnerDensLocfit-clone)

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
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    LearnerDensLocfit$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    LearnerDensLocfit$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
# Define the Learner
learner = lrn("dens.locfit")
print(learner)
#> 
#> ── <LearnerDensLocfit> (dens.locfit): Local Density Estimation ─────────────────
#> • Model: -
#> • Parameters: list()
#> • Packages: mlr3, mlr3proba, locfit, and distr6
#> • Predict Types: [pdf]
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
#> LocFitDens() 

# Make predictions for the test rows
predictions = learner$predict(task, row_ids = ids$test)

# Score the predictions
predictions$score()
#> dens.logloss 
#>     1.062013 
```
