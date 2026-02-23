# Integrated Calibration Index Survival Measure

Calculates the Integrated Calibration Index (ICI), which evaluates
**point-calibration** (i.e. at a specific time point), see Austin et al.
(2020).

## Details

Each individual \\i\\ from the test set, has an observed survival
outcome \\(t_i, \delta_i)\\ (time and censoring indicator) and predicted
survival function \\S_i(t)\\. The predicted probability of an event
occurring before a specific time point \\t_0\\, is defined as
\\\hat{P_i}(t_0) = F_i(t_0) = 1 - S_i(t_0)\\.

Using hazard regression (via the
[polspline](https://CRAN.R-project.org/package=polspline) R package), a
*smoothed* calibration curve is estimated by fitting the following
model: \$\$log(h(t)) = g(log(− log(1 − \hat{P}\_{t_0})), t)\$\$

Note that we substitute probabilities \\\hat{P}\_{t_0} = 0\\ with a
small \\\epsilon\\ number to avoid arithmetic issues (\\log(0)\\). Same
with \\\hat{P}\_{t_0} = 1\\, we use \\1 - \epsilon\\. From this model,
the *smoothed* probability of occurrence at \\t_0\\ for observation
\\i\\ is obtained as \\\hat{P}\_i^c(t_0)\\.

The **Integrated Calibration Index** is then computed across the \\N\\
test set observations as: \$\$ICI = \frac{1}{N} \sum\_{i=1}^N \|
\hat{P}\_i^c(t_0) - \hat{P}\_i(t_0) \|\$\$

Therefore, a perfect calibration (smoothed probabilities match predicted
probabilities for all observations) yields \\ICI = 0\\, while the worst
possible score is \\ICI = 1\\.

## Dictionary

This [Measure](https://mlr3.mlr-org.com/reference/Measure.html) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.html) or
with the associated sugar function
[msr()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    MeasureSurvICI$new()
    mlr_measures$get("surv.calib_index")
    msr("surv.calib_index")

## Parameters

|        |           |         |                     |                  |
|--------|-----------|---------|---------------------|------------------|
| Id     | Type      | Default | Levels              | Range            |
| time   | numeric   | \-      |                     | \\\[0, \infty)\\ |
| eps    | numeric   | 1e-04   |                     | \\\[0, 1\]\\     |
| method | character | ICI     | ICI, E50, E90, Emax | \-               |
| na.rm  | logical   | TRUE    | TRUE, FALSE         | \-               |

## Meta Information

- Type: `"surv"`

- Range: \\\[0, 1\]\\

- Minimize: `TRUE`

- Required prediction: `distr`

## Parameter details

- `eps` (`numeric(1)`)  
  Very small number to substitute near-zero values in order to prevent
  errors in e.g. log(0) and/or division-by-zero calculations. Default
  value is 1e-04.

&nbsp;

- `time` (`numeric(1)`)  
  The specific time point \\t_0\\ at which calibration is evaluated. If
  `NULL`, the median observed time from the test set is used.

- `method` (`character(1)`)  
  Specifies the summary statistic used to calculate the final
  calibration score.

  - `"ICI"` (default): Uses the mean of absolute differences \\\|
    \hat{P}\_i^c(t_0) - \hat{P}\_i(t_0) \|\\ across all observations.

  - `"E50"`: Uses the median of absolute differences instead of the
    mean.

  - `"E90"`: Uses the 90th percentile of absolute differences,
    emphasizing higher deviations.

  - `"Emax"`: Uses the maximum absolute difference, capturing the
    largest discrepancy between predicted and smoothed probabilities.

- `na.rm` (`logical(1)`)  
  If `TRUE` (default) then removes any NAs/NaNs in the smoothed
  probabilities \\\hat{P}\_i^c(t_0)\\ that may arise. A warning is
  issued nonetheless in such cases.

## References

Austin, C. P, Harrell, E. F, van Klaveren, David (2020). “Graphical
calibration curves and the integrated calibration index (ICI) for
survival models.” *Statistics in Medicine*, **39**(21), 2714. ISSN
10970258, [doi:10.1002/SIM.8570](https://doi.org/10.1002/SIM.8570) ,
<https://pmc.ncbi.nlm.nih.gov/articles/PMC7497089/>.

## See also

Other survival measures:
[`mlr_measures_surv.calib_alpha`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_alpha.md),
[`mlr_measures_surv.calib_beta`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_beta.md),
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
[`mlr_measures_surv.uno_auc`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_auc.md),
[`mlr_measures_surv.uno_tnr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_tnr.md),
[`mlr_measures_surv.uno_tpr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_tpr.md),
[`mlr_measures_surv.xu_r2`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.xu_r2.md)

Other calibration survival measures:
[`mlr_measures_surv.calib_alpha`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_alpha.md),
[`mlr_measures_surv.calib_beta`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_beta.md),
[`mlr_measures_surv.dcalib`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.dcalib.md)

Other distr survival measures:
[`mlr_measures_surv.calib_alpha`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_alpha.md),
[`mlr_measures_surv.dcalib`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.dcalib.md),
[`mlr_measures_surv.graf`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.graf.md),
[`mlr_measures_surv.intlogloss`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.intlogloss.md),
[`mlr_measures_surv.logloss`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.logloss.md),
[`mlr_measures_surv.rcll`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.rcll.md),
[`mlr_measures_surv.schmid`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.schmid.md)

## Super classes

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.html) -\>
[`mlr3proba::MeasureSurv`](https://mlr3proba.mlr-org.com/reference/MeasureSurv.md)
-\> `MeasureSurvICI`

## Methods

### Public methods

- [`MeasureSurvICI$new()`](#method-MeasureSurvICI-new)

- [`MeasureSurvICI$clone()`](#method-MeasureSurvICI-clone)

Inherited methods

- [`mlr3::Measure$aggregate()`](https://mlr3.mlr-org.com/reference/Measure.html#method-aggregate)
- [`mlr3::Measure$format()`](https://mlr3.mlr-org.com/reference/Measure.html#method-format)
- [`mlr3::Measure$help()`](https://mlr3.mlr-org.com/reference/Measure.html#method-help)
- [`mlr3::Measure$print()`](https://mlr3.mlr-org.com/reference/Measure.html#method-print)
- [`mlr3::Measure$score()`](https://mlr3.mlr-org.com/reference/Measure.html#method-score)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://rdrr.io/pkg/R6/man/R6Class.html) class.

#### Usage

    MeasureSurvICI$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureSurvICI$clone(deep = FALSE)

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

# ICI at median test set time
p$score(msr("surv.calib_index"))
#> surv.calib_index 
#>        0.1535021 

# ICI at specific time point
p$score(msr("surv.calib_index", time = 365))
#> surv.calib_index 
#>        0.2057292 

# E50 at specific time point
p$score(msr("surv.calib_index", method = "E50", time = 365))
#> surv.calib_index 
#>        0.2110995 
```
