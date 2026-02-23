# Van Houwelingen's Calibration Alpha Survival Measure

This calibration method is defined by estimating \$\$\hat{\alpha} =
\frac{\sum\_{i=1}^n \delta_i}{\sum\_{i=1}^n H_i(T_i)}\$\$ where
\\\delta\\ is the observed censoring indicator from the test data \\n\\
observations), \\H_i\\ is the predicted cumulative hazard, and \\T_i\\
is the observed survival time (event or censoring).

The standard error is given by \$\$\hat{\alpha\_{se}} = e^{1/\sqrt{\sum
\delta_i}}\$\$

The model is well calibrated if the estimated \\\hat{\alpha}\\
coefficient (returned score) is equal to 1.

## Dictionary

This [Measure](https://mlr3.mlr-org.com/reference/Measure.html) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.html) or
with the associated sugar function
[msr()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    MeasureSurvCalibrationAlpha$new()
    mlr_measures$get("surv.calib_alpha")
    msr("surv.calib_alpha")

## Parameters

|          |           |         |             |                       |
|----------|-----------|---------|-------------|-----------------------|
| Id       | Type      | Default | Levels      | Range                 |
| eps      | numeric   | 0.001   |             | \\\[0, 1\]\\          |
| se       | logical   | FALSE   | TRUE, FALSE | \-                    |
| method   | character | ratio   | ratio, diff | \-                    |
| truncate | numeric   | Inf     |             | \\(-\infty, \infty)\\ |

## Meta Information

- Type: `"surv"`

- Range: \\(-\infty, \infty)\\

- Minimize: `FALSE`

- Required prediction: `distr`

## Parameter details

- `eps` (`numeric(1)`)  
  Very small number to substitute near-zero values in order to prevent
  errors in e.g. log(0) and/or division-by-zero calculations. Default
  value is 0.001.

&nbsp;

- `se` (`logical(1)`)  
  If `TRUE` then return standard error of the measure, otherwise the
  score itself (default).

- `method` (`character(1)`)  
  Returns \\\hat{\alpha}\\ if equal to `ratio` (default) and
  \\\|1-\hat{\alpha}\|\\ if equal to `diff`. With `diff`, the output
  score can be minimized and for example be used for tuning purposes.
  This parameter takes effect only if `se` is `FALSE`.

- `truncate` (`double(1)`)  
  This parameter controls the upper bound of the output score. We use
  `truncate = Inf` by default (so no truncation) and it's up to the user
  **to set this up reasonably** given the chosen `method`. Note that
  truncation may severely limit automated tuning with this measure using
  `method = diff`.

## References

Van Houwelingen, C. H (2000). “Validation, calibration, revision and
combination of prognostic survival models.” *Statistics in Medicine*,
**19**(24), 3401–3415.
[doi:10.1002/1097-0258(20001230)19:24\<3401::AID-SIM554\>3.0.CO;2-2](https://doi.org/10.1002/1097-0258%2820001230%2919%3A24%3C3401%3A%3AAID-SIM554%3E3.0.CO%3B2-2)
.

## See also

Other survival measures:
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
[`mlr_measures_surv.uno_auc`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_auc.md),
[`mlr_measures_surv.uno_tnr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_tnr.md),
[`mlr_measures_surv.uno_tpr`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.uno_tpr.md),
[`mlr_measures_surv.xu_r2`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.xu_r2.md)

Other calibration survival measures:
[`mlr_measures_surv.calib_beta`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_beta.md),
[`mlr_measures_surv.calib_index`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_index.md),
[`mlr_measures_surv.dcalib`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.dcalib.md)

Other distr survival measures:
[`mlr_measures_surv.calib_index`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_index.md),
[`mlr_measures_surv.dcalib`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.dcalib.md),
[`mlr_measures_surv.graf`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.graf.md),
[`mlr_measures_surv.intlogloss`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.intlogloss.md),
[`mlr_measures_surv.logloss`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.logloss.md),
[`mlr_measures_surv.rcll`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.rcll.md),
[`mlr_measures_surv.schmid`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.schmid.md)

## Super classes

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.html) -\>
[`mlr3proba::MeasureSurv`](https://mlr3proba.mlr-org.com/reference/MeasureSurv.md)
-\> `MeasureSurvCalibrationAlpha`

## Methods

### Public methods

- [`MeasureSurvCalibrationAlpha$new()`](#method-MeasureSurvCalibrationAlpha-new)

- [`MeasureSurvCalibrationAlpha$clone()`](#method-MeasureSurvCalibrationAlpha-clone)

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

    MeasureSurvCalibrationAlpha$new(method = "ratio")

#### Arguments

- `method`:

  defines which output score to return, see "Parameter details" section.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureSurvCalibrationAlpha$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
