# Uno's AUC Survival Measure

Calls
[`survAUC::AUC.uno()`](https://fbertran.github.io/survAUC/reference/survAUC_Uno.html).

Assumes random censoring.

## Details

All measures implemented from
[survAUC](https://CRAN.R-project.org/package=survAUC) should be used
with care, we are aware of problems in implementation that sometimes
cause fatal errors in R. In future updates some of these measures may be
re-written and implemented directly in `mlr3proba`.

## Dictionary

This [Measure](https://mlr3.mlr-org.com/reference/Measure.html) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.html) or
with the associated sugar function
[msr()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    MeasureSurvUnoAUC$new()
    mlr_measures$get("surv.uno_auc")
    msr("surv.uno_auc")

## Parameters

|            |         |         |             |
|------------|---------|---------|-------------|
| Id         | Type    | Default | Levels      |
| integrated | logical | TRUE    | TRUE, FALSE |
| times      | untyped | \-      |             |

## Meta Information

- Type: `"surv"`

- Range: \\\[0, 1\]\\

- Minimize: `FALSE`

- Required prediction: `lp`

## Parameter details

- `integrated` (`logical(1)`)  
  If `TRUE` (default), returns the integrated score (eg across time
  points); otherwise, not integrated (eg at a single time point).

&nbsp;

- `times` ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  If `integrated == TRUE` then a vector of time-points over which to
  integrate the score. If `integrated == FALSE` then a single time point
  at which to return the score.

## References

Uno H, Cai T, Tian L, Wei LJ (2007). “Evaluating Prediction Rules
fort-Year Survivors With Censored Regression Models.” *Journal of the
American Statistical Association*, **102**(478), 527–537.
[doi:10.1198/016214507000000149](https://doi.org/10.1198/016214507000000149)
.

## See also

Other survival measures:
[`mlr_measures_surv.calib_alpha`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_alpha.md),
[`mlr_measures_surv.calib_beta`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_beta.md),
[`mlr_measures_surv.calib_index`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_index.md),
[`mlr_measures_surv.chambless_auc`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.chambless_auc.md),
[`mlr_measures_surv.cindex`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.cindex.md),
[`mlr_measures_surv.dcalib`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.dcalib.md),
[`mlr_measures_surv.graf`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.graf.md),
[`mlr_measures_surv.hung_auc`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.hung_auc.md),
[`mlr_measures_surv.intlogloss`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.intlogloss.md),
[`mlr_measures_surv.logloss`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.logloss.md),
[`mlr_measures_surv.mae`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.mae.md),
[`mlr_measures_surv.mse`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.mse.md),
[`mlr_measures_surv.nagelk_r2`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.nagelk_r2.md),
[`mlr_measures_surv.oquigley_r2`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.oquigley_r2.md),
[`mlr_measures_surv.rcll`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.rcll.md),
[`mlr_measures_surv.rmse`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.rmse.md),
[`mlr_measures_surv.schmid`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.schmid.md),
[`mlr_measures_surv.song_auc`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.song_auc.md),
[`mlr_measures_surv.song_tnr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.song_tnr.md),
[`mlr_measures_surv.song_tpr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.song_tpr.md),
[`mlr_measures_surv.uno_tnr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_tnr.md),
[`mlr_measures_surv.uno_tpr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_tpr.md),
[`mlr_measures_surv.xu_r2`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.xu_r2.md)

Other AUC survival measures:
[`mlr_measures_surv.chambless_auc`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.chambless_auc.md),
[`mlr_measures_surv.hung_auc`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.hung_auc.md),
[`mlr_measures_surv.song_auc`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.song_auc.md),
[`mlr_measures_surv.song_tnr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.song_tnr.md),
[`mlr_measures_surv.song_tpr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.song_tpr.md),
[`mlr_measures_surv.uno_tnr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_tnr.md),
[`mlr_measures_surv.uno_tpr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_tpr.md)

Other lp survival measures:
[`mlr_measures_surv.calib_beta`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_beta.md),
[`mlr_measures_surv.chambless_auc`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.chambless_auc.md),
[`mlr_measures_surv.hung_auc`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.hung_auc.md),
[`mlr_measures_surv.nagelk_r2`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.nagelk_r2.md),
[`mlr_measures_surv.oquigley_r2`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.oquigley_r2.md),
[`mlr_measures_surv.song_auc`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.song_auc.md),
[`mlr_measures_surv.song_tnr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.song_tnr.md),
[`mlr_measures_surv.song_tpr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.song_tpr.md),
[`mlr_measures_surv.uno_tnr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_tnr.md),
[`mlr_measures_surv.uno_tpr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_tpr.md),
[`mlr_measures_surv.xu_r2`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.xu_r2.md)

## Super classes

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.html) -\>
[`mlr3proba::MeasureSurv`](https://mlr3proba.mlr-org.com/reference/MeasureSurv.md)
-\>
[`mlr3proba::MeasureSurvAUC`](https://mlr3proba.mlr-org.com/reference/MeasureSurvAUC.md)
-\> `MeasureSurvUnoAUC`

## Methods

### Public methods

- [`MeasureSurvUnoAUC$new()`](#method-MeasureSurvUnoAUC-new)

- [`MeasureSurvUnoAUC$clone()`](#method-MeasureSurvUnoAUC-clone)

Inherited methods

- [`mlr3::Measure$aggregate()`](https://mlr3.mlr-org.com/reference/Measure.html#method-aggregate)
- [`mlr3::Measure$format()`](https://mlr3.mlr-org.com/reference/Measure.html#method-format)
- [`mlr3::Measure$help()`](https://mlr3.mlr-org.com/reference/Measure.html#method-help)
- [`mlr3::Measure$print()`](https://mlr3.mlr-org.com/reference/Measure.html#method-print)
- [`mlr3::Measure$score()`](https://mlr3.mlr-org.com/reference/Measure.html#method-score)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    MeasureSurvUnoAUC$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureSurvUnoAUC$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)

# Define a survival Task
task = tsk("lung")

# Create train and test set
part = partition(task)

# Train Cox learner on the train set
cox = lrn("surv.coxph")
cox$train(task, row_ids = part$train)

# Make predictions for the test set
p = cox$predict(task, row_ids = part$test)

# Integrated AUC score
p$score(msr("surv.uno_auc"), task = task,
        train_set = part$train, learner = cox)
#> surv.uno_auc 
#>    0.5788296 

# AUC at specific time point
p$score(msr("surv.uno_auc", times = 600), task = task,
        train_set = part$train, learner = cox)
#> surv.uno_auc 
#>    0.5578532 

# Integrated AUC at specific time points
p$score(msr("surv.uno_auc", times = c(100, 200, 300, 400, 500)),
        task = task, train_set = part$train, learner = cox)
#> surv.uno_auc 
#>    0.6051487 
```
