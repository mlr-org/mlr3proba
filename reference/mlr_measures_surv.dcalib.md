# D-Calibration Survival Measure

**\[experimental\]**

This calibration method is defined by calculating the following
statistic: \$\$s = B/n \sum_i (P_i - n/B)^2\$\$ where \\B\\ is number of
'buckets' (that equally divide \\\[0,1\]\\ into intervals), \\n\\ is the
number of predictions, and \\P_i\\ is the observed proportion of
observations in the \\i\\th interval. An observation is assigned to the
\\i\\th bucket, if its predicted survival probability at the time of
event falls within the corresponding interval. This statistic assumes
that censoring time is independent of death time.

A model is well D-calibrated if \\s \sim Unif(B)\\, tested with
`chisq.test` (\\p \> 0.05\\ if well-calibrated, i.e. higher p-values are
preferred). Model \\i\\ is better calibrated than model \\j\\ if \\s(i)
\< s(j)\\, meaning that *lower values* of this measure are preferred.

## Details

This measure can either return the test statistic or the p-value from
the `chisq.test`. The former is useful for model comparison whereas the
latter is useful for determining if a model is well-calibrated. If
`chisq = FALSE` and `s` is the predicted value then you can manually
compute the p.value with `pchisq(s, B - 1, lower.tail = FALSE)`.

**NOTE**: This measure is still experimental both theoretically and in
implementation. Results should therefore only be taken as an indicator
of performance and not for conclusive judgements about model
calibration.

## Dictionary

This [Measure](https://mlr3.mlr-org.com/reference/Measure.html) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.html) or
with the associated sugar function
[msr()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    MeasureSurvDCalibration$new()
    mlr_measures$get("surv.dcalib")
    msr("surv.dcalib")

## Parameters

|          |         |         |             |                  |
|----------|---------|---------|-------------|------------------|
| Id       | Type    | Default | Levels      | Range            |
| B        | integer | 10      |             | \\\[1, \infty)\\ |
| chisq    | logical | FALSE   | TRUE, FALSE | \-               |
| truncate | numeric | Inf     |             | \\\[0, \infty)\\ |

## Meta Information

- Type: `"surv"`

- Range: \\\[0, \infty)\\

- Minimize: `TRUE`

- Required prediction: `distr`

## Parameter details

- `B` (`integer(1)`)  
  Number of buckets to test for uniform predictions over. Default of
  `10` is recommended by Haider et al. (2020). Changing this parameter
  affects `truncate`.

- `chisq` (`logical(1)`)  
  If `TRUE` returns the p-value of the corresponding chisq.test instead
  of the measure. Default is `FALSE` and returns the statistic `s`. You
  can manually get the p-value by executing
  `pchisq(s, B - 1, lower.tail = FALSE)`. The null hypothesis is that
  the model is D-calibrated.

- `truncate` (`double(1)`)  
  This parameter controls the upper bound of the output statistic, when
  `chisq` is `FALSE`. We use `truncate = Inf` by default but values
  between \\10-16\\ are sufficient for most purposes, which correspond
  to p-values of \\0.35-0.06\\ for the `chisq.test` using the default
  \\B = 10\\ buckets. Values \\B \> 10\\ translate to even lower
  p-values and thus less D-calibrated models. If the number of buckets
  \\B\\ changes, you probably will want to change the `truncate` value
  as well to correspond to the same p-value significance. Note that
  truncation may severely limit automated tuning with this measure.

## References

Haider, Humza, Hoehn, Bret, Davis, Sarah, Greiner, Russell (2020).
“Effective Ways to Build and Evaluate Individual Survival
Distributions.” *Journal of Machine Learning Research*, **21**(85),
1–63. <https://jmlr.org/papers/v21/18-772.html>.

## See also

Other survival measures:
[`mlr_measures_surv.calib_alpha`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_alpha.md),
[`mlr_measures_surv.calib_beta`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_beta.md),
[`mlr_measures_surv.calib_index`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_index.md),
[`mlr_measures_surv.chambless_auc`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.chambless_auc.md),
[`mlr_measures_surv.cindex`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.cindex.md),
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
[`mlr_measures_surv.calib_index`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_index.md)

Other distr survival measures:
[`mlr_measures_surv.calib_alpha`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_alpha.md),
[`mlr_measures_surv.calib_index`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_index.md),
[`mlr_measures_surv.graf`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.graf.md),
[`mlr_measures_surv.intlogloss`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.intlogloss.md),
[`mlr_measures_surv.logloss`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.logloss.md),
[`mlr_measures_surv.rcll`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.rcll.md),
[`mlr_measures_surv.schmid`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.schmid.md)

## Super classes

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.html) -\>
[`mlr3proba::MeasureSurv`](https://mlr3proba.mlr-org.com/reference/MeasureSurv.md)
-\> `MeasureSurvDCalibration`

## Methods

### Public methods

- [`MeasureSurvDCalibration$new()`](#method-MeasureSurvDCalibration-new)

- [`MeasureSurvDCalibration$clone()`](#method-MeasureSurvDCalibration-clone)

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

    MeasureSurvDCalibration$new()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureSurvDCalibration$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
