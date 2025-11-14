# Concordance Statistics Survival Measure

Calculates weighted concordance statistics, which, depending on the
chosen weighting method (`weight_meth`) and tied times parameter
(`tiex`), are equivalent to several proposed methods. By default, no
weighting is applied and this is equivalent to Harrell's C-index.

## Details

For the Kaplan-Meier estimate of the **training survival** distribution
(\\S\\), and the Kaplan-Meier estimate of the **training censoring**
distribution (\\G\\), we have the following options for time-independent
concordance statistics (C-indexes) given the weighted method:

`weight_meth`:

- `"I"` = No weighting. (Harrell)

- `"GH"` = Gonen and Heller's Concordance Index

- `"G"` = Weights concordance by \\1/G\\.

- `"G2"` = Weights concordance by \\1/G^2\\. (Uno et al.)

- `"SG"` = Weights concordance by \\S/G\\ (Shemper et al.)

- `"S"` = Weights concordance by \\S\\ (Peto and Peto)

The last three require training data. `"GH"` is only applicable to
[LearnerSurvCoxPH](https://mlr3proba.mlr-org.com/reference/mlr_learners_surv.coxph.md).

The implementation is slightly different from
[survival::concordance](https://rdrr.io/pkg/survival/man/concordance.html).
Firstly this implementation is faster, and secondly the weights are
computed on the training dataset whereas in
[survival::concordance](https://rdrr.io/pkg/survival/man/concordance.html)
the weights are computed on the same testing data.

## Dictionary

This [Measure](https://mlr3.mlr-org.com/reference/Measure.html) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.html) or
with the associated sugar function
[msr()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    MeasureSurvCindex$new()
    mlr_measures$get("surv.cindex")
    msr("surv.cindex")

## Parameters

|             |           |         |                     |                  |
|-------------|-----------|---------|---------------------|------------------|
| Id          | Type      | Default | Levels              | Range            |
| t_max       | numeric   | \-      |                     | \\\[0, \infty)\\ |
| p_max       | numeric   | \-      |                     | \\\[0, 1\]\\     |
| weight_meth | character | I       | I, G, G2, SG, S, GH | \-               |
| tiex        | numeric   | 0.5     |                     | \\\[0, 1\]\\     |
| eps         | numeric   | 0.001   |                     | \\\[0, 1\]\\     |

## Meta Information

- Type: `"surv"`

- Range: \\\[0, 1\]\\

- Minimize: `FALSE`

- Required prediction: `crank`

## Parameter details

- `eps` (`numeric(1)`)  
  Very small number to substitute near-zero values in order to prevent
  errors in e.g. log(0) and/or division-by-zero calculations. Default
  value is 0.001.

&nbsp;

- `t_max` (`numeric(1)`)  
  Cutoff time (i.e. time horizon) to evaluate concordance up to.

- `p_max` (`numeric(1)`)  
  The proportion of censoring to evaluate concordance up to in the given
  dataset. When `t_max` is specified, this parameter is ignored.

- `weight_meth` (`character(1)`)  
  Method for weighting concordance. Default `"I"` is Harrell's C. See
  details.

- `tiex` (`numeric(1)`)  
  Weighting applied to tied rankings, default is to give them half (0.5)
  weighting.

## References

Peto, Richard, Peto, Julian (1972). “Asymptotically efficient rank
invariant test procedures.” *Journal of the Royal Statistical Society:
Series A (General)*, **135**(2), 185–198.

Harrell, E F, Califf, M R, Pryor, B D, Lee, L K, Rosati, A R (1982).
“Evaluating the yield of medical tests.” *Jama*, **247**(18), 2543–2546.

Gonen M, Heller G (2005). “Concordance probability and discriminatory
power in proportional hazards regression.” *Biometrika*, **92**(4),
965–970.
[doi:10.1093/biomet/92.4.965](https://doi.org/10.1093/biomet/92.4.965) .

Schemper, Michael, Wakounig, Samo, Heinze, Georg (2009). “The estimation
of average hazard ratios by weighted Cox regression.” *Statistics in
Medicine*, **28**(19), 2473–2489.
[doi:10.1002/sim.3623](https://doi.org/10.1002/sim.3623) .

Uno H, Cai T, Pencina MJ, D'Agostino RB, Wei LJ (2011). “On the
C-statistics for evaluating overall adequacy of risk prediction
procedures with censored survival data.” *Statistics in Medicine*,
n/a–n/a. [doi:10.1002/sim.4154](https://doi.org/10.1002/sim.4154) .

## See also

Other survival measures:
[`mlr_measures_surv.calib_alpha`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_alpha.md),
[`mlr_measures_surv.calib_beta`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_beta.md),
[`mlr_measures_surv.calib_index`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_index.md),
[`mlr_measures_surv.chambless_auc`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.chambless_auc.md),
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

## Super classes

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.html) -\>
[`mlr3proba::MeasureSurv`](https://mlr3proba.mlr-org.com/reference/MeasureSurv.md)
-\> `MeasureSurvCindex`

## Methods

### Public methods

- [`MeasureSurvCindex$new()`](#method-MeasureSurvCindex-new)

- [`MeasureSurvCindex$clone()`](#method-MeasureSurvCindex-clone)

Inherited methods

- [`mlr3::Measure$aggregate()`](https://mlr3.mlr-org.com/reference/Measure.html#method-aggregate)
- [`mlr3::Measure$format()`](https://mlr3.mlr-org.com/reference/Measure.html#method-format)
- [`mlr3::Measure$help()`](https://mlr3.mlr-org.com/reference/Measure.html#method-help)
- [`mlr3::Measure$print()`](https://mlr3.mlr-org.com/reference/Measure.html#method-print)
- [`mlr3::Measure$score()`](https://mlr3.mlr-org.com/reference/Measure.html#method-score)

------------------------------------------------------------------------

### Method `new()`

This is an abstract class that should not be constructed directly.

#### Usage

    MeasureSurvCindex$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureSurvCindex$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
library(mlr3)
task = tsk("rats")
learner = lrn("surv.coxph")
part = partition(task) # train/test split
learner$train(task, part$train)
p = learner$predict(task, part$test)

# Harrell's C-index
p$score(msr("surv.cindex")) # same as `p$score()`
#> surv.cindex 
#>   0.7654723 

# Uno's C-index
p$score(msr("surv.cindex", weight_meth = "G2"),
        task = task, train_set = part$train)
#> surv.cindex 
#>   0.8161425 

# Harrell's C-index evaluated up to a specific time horizon
p$score(msr("surv.cindex", t_max = 97))
#> surv.cindex 
#>   0.7285714 

# Harrell's C-index evaluated up to the time corresponding to 30% of censoring
p$score(msr("surv.cindex", p_max = 0.3))
#> surv.cindex 
#>   0.7285714 
```
