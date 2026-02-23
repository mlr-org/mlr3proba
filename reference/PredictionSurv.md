# Prediction Object for Survival

This object stores the predictions returned by a learner of class
[LearnerSurv](https://mlr3proba.mlr-org.com/reference/LearnerSurv.md).

The `task_type` is set to `"surv"`.

For accessing survival and hazard functions, as well as other complex
methods from a PredictionSurv object, see public methods on
[`distr6::ExoticStatistics()`](https://xoopr.github.io/distr6/reference/ExoticStatistics.html)
and example below.

## See also

Other Prediction:
[`PredictionDens`](https://mlr3proba.mlr-org.com/reference/PredictionDens.md)

## Super class

[`mlr3::Prediction`](https://mlr3.mlr-org.com/reference/Prediction.html)
-\> `PredictionSurv`

## Active bindings

- `truth`:

  (`Surv`)  
  True (observed) outcome.

- `crank`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Access the stored predicted continuous ranking.

- `distr`:

  ([distr6::Matdist](https://xoopr.github.io/distr6/reference/Matdist.html)\|[distr6::Arrdist](https://xoopr.github.io/distr6/reference/Arrdist.html)\|[distr6::VectorDistribution](https://xoopr.github.io/distr6/reference/VectorDistribution.html))  
  Convert the stored survival array or matrix to a survival
  distribution.

- `lp`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Access the stored predicted linear predictor.

- `response`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Access the stored predicted survival time.

## Methods

### Public methods

- [`PredictionSurv$new()`](#method-PredictionSurv-new)

- [`PredictionSurv$clone()`](#method-PredictionSurv-clone)

Inherited methods

- [`mlr3::Prediction$filter()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-filter)
- [`mlr3::Prediction$format()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-format)
- [`mlr3::Prediction$help()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-help)
- [`mlr3::Prediction$obs_loss()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-obs_loss)
- [`mlr3::Prediction$print()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-print)
- [`mlr3::Prediction$score()`](https://mlr3.mlr-org.com/reference/Prediction.html#method-score)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://rdrr.io/pkg/R6/man/R6Class.html) class.

#### Usage

    PredictionSurv$new(
      task = NULL,
      row_ids = task$row_ids,
      truth = task$truth(),
      crank = NULL,
      distr = NULL,
      lp = NULL,
      response = NULL,
      check = TRUE
    )

#### Arguments

- `task`:

  ([TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md))  
  Task, used to extract defaults for `row_ids` and `truth`.

- `row_ids`:

  ([`integer()`](https://rdrr.io/r/base/integer.html))  
  Row ids of the predicted observations, i.e. the row ids of the test
  set.

- `truth`:

  ([`survival::Surv()`](https://rdrr.io/pkg/survival/man/Surv.html))  
  True (observed) response.

- `crank`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Numeric vector of predicted continuous rankings (or relative risks).
  One element for each observation in the test set. For a pair of
  continuous ranks, a higher rank indicates that the observation is more
  likely to experience the event.

- `distr`:

  (`matrix()|[distr6::Arrdist]|[distr6::Matdist]|[distr6::VectorDistribution]`)  
  Either a matrix of predicted survival probabilities, a
  [distr6::VectorDistribution](https://xoopr.github.io/distr6/reference/VectorDistribution.html),
  a
  [distr6::Matdist](https://xoopr.github.io/distr6/reference/Matdist.html)
  or an
  [distr6::Arrdist](https://xoopr.github.io/distr6/reference/Arrdist.html).
  If a matrix/array then column names must be given and correspond to
  survival times. Rows of matrix correspond to individual predictions.
  It is advised that the first column should be time `0` with all
  entries `1` and the last with all entries `0`. If a
  `VectorDistribution` then each distribution in the vector should
  correspond to a predicted survival distribution.

- `lp`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Numeric vector of linear predictor scores. One element for each
  observation in the test set. \\lp = X\beta\\ where \\X\\ is a matrix
  of covariates and \\\beta\\ is a vector of estimated coefficients.

- `response`:

  ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  Numeric vector of predicted survival times. One element for each
  observation in the test set.

- `check`:

  (`logical(1)`)  
  If `TRUE`, performs argument checks and predict type conversions.

#### Details

Upon **initialization**, if the `distr` input is a
[Distribution](https://xoopr.github.io/distr6/reference/Distribution.html),
we try to coerce it either to a survival matrix or a survival array and
store it in the `$data$distr` slot for internal use.

If the stored `$data$distr` is a
[Distribution](https://xoopr.github.io/distr6/reference/Distribution.html)
object, the active field `$distr` (**external user API**) returns it
without modification. Otherwise, if `$data$distr` is a survival matrix
or array, `$distr` constructs a distribution out of the `$data$distr`
object, which will be a
[Matdist](https://xoopr.github.io/distr6/reference/Matdist.html) or
[Arrdist](https://xoopr.github.io/distr6/reference/Arrdist.html)
respectively.

Note that if a survival 3d array is stored in `$data$distr`, the
`$distr` field returns an
[Arrdist](https://xoopr.github.io/distr6/reference/Arrdist.html)
initialized with `which.curve = 0.5` by default (i.e. the median curve).
This means that measures that require a `distr` prediction like
[MeasureSurvGraf](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.graf.md),
[MeasureSurvRCLL](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.rcll.md),
etc. will use the median survival probabilities. Note that it is
possible to manually change `which.curve` after construction of the
predicted distribution but we advise against this as it may lead to
inconsistent results.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PredictionSurv$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = tsk("rats")
learner = lrn("surv.kaplan")
p = learner$train(task, row_ids = 1:26)$predict(task, row_ids = 27:30)
head(as.data.table(p))
#>    row_ids  time status     crank     distr
#>      <int> <num> <lgcl>     <num>    <list>
#> 1:      27   104  FALSE 0.4706486 <list[1]>
#> 2:      28   104  FALSE 0.4706486 <list[1]>
#> 3:      29   104  FALSE 0.4706486 <list[1]>
#> 4:      30    98  FALSE 0.4706486 <list[1]>

p$distr # distr6::Matdist class (test obs x time points)
#> Matdist(4x12) 

# survival probabilities of the 4 test rats at two time points
p$distr$survival(c(20, 100))
#>          [,1]      [,2]      [,3]      [,4]
#> 20  1.0000000 1.0000000 1.0000000 1.0000000
#> 100 0.9615385 0.9615385 0.9615385 0.9615385
```
