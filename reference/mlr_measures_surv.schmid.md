# Integrated Schmid Score Survival Measure

Calculates the **Integrated Schmid Score** (ISS), aka integrated
absolute loss.

## Details

This measure has two dimensions: (test set) observations and time
points. For a specific individual \\i\\ from the test set, with observed
survival outcome \\(t_i, \delta_i)\\ (time and censoring indicator) and
predicted survival function \\S_i(t)\\, the *observation-wise* estimator
of the loss, integrated across the time dimension up to the time cutoff
\\\tau^\*\\, is:

\$\$L\_{ISS}(S_i, t_i, \delta_i) = \int^{\tau^\*}\_0 \frac{S_i(\tau)
\text{I}(t_i \leq \tau, \delta=1)}{G(t_i)} + \frac{(1-S_i(\tau))
\text{I}(t_i \> \tau)}{G(\tau)} \\ d\tau\$\$

where \\G\\ is the Kaplan-Meier estimate of the censoring distribution.

The implementation uses the trapezoidal rule to approximate the integral
over time and the integral is normalized by the range of available
evaluation times (\\\tau\_{\text{max}} - \tau\_{\text{min}}\\).

To get a single score across all \\N\\ observations of the test set, we
return the average of the time-integrated observation-wise scores:
\$\$\sum\_{i=1}^N L(S_i, t_i, \delta_i) / N\$\$

## Dictionary

This [Measure](https://mlr3.mlr-org.com/reference/Measure.html) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.html) or
with the associated sugar function
[msr()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    MeasureSurvSchmid$new()
    mlr_measures$get("surv.schmid")
    msr("surv.schmid")

## Parameters

|            |         |         |             |                  |
|------------|---------|---------|-------------|------------------|
| Id         | Type    | Default | Levels      | Range            |
| integrated | logical | TRUE    | TRUE, FALSE | \-               |
| times      | untyped | \-      |             | \-               |
| t_max      | numeric | \-      |             | \\\[0, \infty)\\ |
| p_max      | numeric | \-      |             | \\\[0, 1\]\\     |
| eps        | numeric | 0.001   |             | \\\[0, 1\]\\     |
| ERV        | logical | FALSE   | TRUE, FALSE | \-               |

## Meta Information

- Type: `"surv"`

- Range: \\\[0, \infty)\\

- Minimize: `TRUE`

- Required prediction: `distr`

## Parameter details

- `integrated` (`logical(1)`)  
  If `TRUE` (default), returns the integrated score (eg across time
  points); otherwise, not integrated (eg at a single time point).

&nbsp;

- `times` ([`numeric()`](https://rdrr.io/r/base/numeric.html))  
  If `integrated == TRUE` then a vector of time-points over which to
  integrate the score. If `integrated == FALSE` then a single time point
  at which to return the score.

&nbsp;

- `t_max` (`numeric(1)`)  
  Cutoff time \\\tau^\*\\ (i.e. time horizon) to evaluate the measure up
  to (truncate \\S(t)\\). Mutually exclusive with `p_max` or `times`.
  It's recommended to set `t_max` to avoid division by `eps`, see "Time
  Cutoff Details" section. If `t_max` is not specified, an `Inf` time
  horizon is assumed.

&nbsp;

- `p_max` (`numeric(1)`)  
  The proportion of censoring to integrate up to in the given dataset.
  Mutually exclusive with `times` or `t_max`.

&nbsp;

- `eps` (`numeric(1)`)  
  Very small number to substitute near-zero values in order to prevent
  errors in e.g. log(0) and/or division-by-zero calculations. Default
  value is 0.001.

&nbsp;

- `ERV` (`logical(1)`)  
  If `TRUE` then the Explained Residual Variation method is applied,
  which means the score is standardized against a Kaplan-Meier baseline.
  Default is `FALSE`.

## Properness

ISS is not a proper scoring rule, see Sonabend et al. (2024) for more
details. The assumptions for consistent estimation of the loss are that
the censoring distribution \\G(t)\\ is independent of the survival
distribution and \\G(t)\\ is fit on a sufficiently large dataset.

## Time points used for evaluation

If the `times` argument is not specified (`NULL`), then the sorted
unique time points from the **test set** are used for evaluation of the
time-integrated score. This was a design decision due to the fact that
different predicted survival distributions \\S(t)\\ usually have a
**discretized time domain** which may differ, i.e. in the case the
survival predictions come from different survival learners. Essentially,
using the same set of time points for the calculation of the score
minimizes the bias that would come from using different time points. We
note that we perform **constant interpolation** of \\S(t)\\ for time
points that fall outside its discretized time domain.

Naturally, if the `times` argument is specified, then exactly these time
points are used for evaluation. A warning is given to the user in case
some of the specified `times` fall outside of the time point range of
the test set. The assumption here is that if the test set is large
enough, it should have a time domain/range similar to the one from the
train set, and therefore time points outside that domain might lead to
unwanted extrapolation of \\S(t)\\.

## Data used for Estimating Censoring Distribution

If `task` and `train_set` are passed to `$score` then \\G(t)\\ is fit
using **all observations** from the train set, otherwise the test set is
used. Using the train set is likely to reduce any bias caused by
calculating parts of the measure on the test data it is evaluating. Also
usually it means that more data is used for fitting the censoring
distribution \\G(t)\\ via the Kaplan-Meier. The training data is
automatically used in scoring resamplings.

## Time Cutoff Details

If `t_max` or `p_max` is given, then the predicted survival function
\\S(t)\\ is truncated at the time cutoff for all observations. This
helps mitigate **inflation of the score** which can occur when an
observation is censored at the last observed time. In such cases, \\G(t)
= 0\\, triggering the use of a small constant `eps` instead, see Kvamme
et al. (2023). Not using a `t_max` can lead to misleading evaluation,
violations of properness and poor optimization outcomes when using this
score for model tuning, see Sonabend et al. (2024).

## Implementation differences

If comparing the integrated Graf score to other packages, e.g.
[pec](https://CRAN.R-project.org/package=pec), results may be very
slightly different as this package uses `survfit` to estimate the
censoring distribution, in line with the Graf 1999 paper; whereas some
other packages use `prodlim` with `reverse = TRUE` (meaning Kaplan-Meier
is not used).

## References

Schemper, Michael, Henderson, Robin (2000). “Predictive Accuracy and
Explained Variation in Cox Regression.” *Biometrics*, **56**, 249–255.
[doi:10.1002/sim.1486](https://doi.org/10.1002/sim.1486) .

Schmid, Matthias, Hielscher, Thomas, Augustin, Thomas, Gefeller, Olaf
(2011). “A Robust Alternative to the Schemper-Henderson Estimator of
Prediction Error.” *Biometrics*, **67**(2), 524–535.
[doi:10.1111/j.1541-0420.2010.01459.x](https://doi.org/10.1111/j.1541-0420.2010.01459.x)
.

Sonabend, Raphael, Zobolas, John, Kopper, Philipp, Burk, Lukas, Bender,
Andreas (2024). “Examining properness in the external validation of
survival models with squared and logarithmic losses.”
<https://arxiv.org/abs/2212.05260v3>.

Kvamme, Havard, Borgan, Ornulf (2023). “The Brier Score under
Administrative Censoring: Problems and a Solution.” *Journal of Machine
Learning Research*, **24**(2), 1–26. ISSN 1533-7928,
[http://jmlr.org/papers/v24/19-1030.html](http://jmlr.org/papers/v24/19-1030.md).

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
[`mlr_measures_surv.song_auc`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.song_auc.md),
[`mlr_measures_surv.song_tnr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.song_tnr.md),
[`mlr_measures_surv.song_tpr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.song_tpr.md),
[`mlr_measures_surv.uno_auc`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_auc.md),
[`mlr_measures_surv.uno_tnr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_tnr.md),
[`mlr_measures_surv.uno_tpr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_tpr.md),
[`mlr_measures_surv.xu_r2`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.xu_r2.md)

Other Probabilistic survival measures:
[`mlr_measures_surv.graf`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.graf.md),
[`mlr_measures_surv.intlogloss`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.intlogloss.md),
[`mlr_measures_surv.logloss`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.logloss.md),
[`mlr_measures_surv.rcll`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.rcll.md)

Other distr survival measures:
[`mlr_measures_surv.calib_alpha`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_alpha.md),
[`mlr_measures_surv.calib_index`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_index.md),
[`mlr_measures_surv.dcalib`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.dcalib.md),
[`mlr_measures_surv.graf`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.graf.md),
[`mlr_measures_surv.intlogloss`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.intlogloss.md),
[`mlr_measures_surv.logloss`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.logloss.md),
[`mlr_measures_surv.rcll`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.rcll.md)

## Super classes

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.html) -\>
[`mlr3proba::MeasureSurv`](https://mlr3proba.mlr-org.com/reference/MeasureSurv.md)
-\> `MeasureSurvSchmid`

## Methods

### Public methods

- [`MeasureSurvSchmid$new()`](#method-MeasureSurvSchmid-new)

- [`MeasureSurvSchmid$clone()`](#method-MeasureSurvSchmid-clone)

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

    MeasureSurvSchmid$new(ERV = FALSE)

#### Arguments

- `ERV`:

  (`logical(1)`)  
  Standardize measure against a Kaplan-Meier baseline (Explained
  Residual Variation)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureSurvSchmid$clone(deep = FALSE)

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

# ISS, G(t) calculated using the test set
p$score(msr("surv.schmid"))
#> surv.schmid 
#>   0.3163985 

# ISS, G(t) calculated using the train set (always recommended)
p$score(msr("surv.schmid"), task = task, train_set = part$train)
#> surv.schmid 
#>   0.3454207 

# ISS, ERV score (comparing with KM baseline)
p$score(msr("surv.schmid", ERV = TRUE), task = task, train_set = part$train)
#> surv.schmid 
#>  0.05421433 

# ISS at specific time point
p$score(msr("surv.schmid", times = 365), task = task, train_set = part$train)
#> surv.schmid 
#>   0.4580939 

# ISS at multiple time points (integrated)
p$score(msr("surv.schmid", times = c(125, 365, 450), integrated = TRUE),
        task = task, train_set = part$train)
#> surv.schmid 
#>    0.387498 

# ISS, use time cutoff
p$score(msr("surv.schmid", t_max = 700), task = task, train_set = part$train)
#> surv.schmid 
#>   0.3692835 

# ISS, use time cutoff corresponding to specific proportion of censoring on the test set
p$score(msr("surv.schmid", p_max = 0.8), task = task, train_set = part$train)
#> surv.schmid 
#>   0.3669306 
```
