# Right-Censored Log Loss Survival Measure

Calculates the right-censored log-likelihood (RCLL) or logarithmic loss,
introduced by Avati et al. (2020).

## Details

The observation-wise RCLL is defined by:

\$\$L\_{RCLL}(S_i, t_i, \delta_i) = -log\[\delta_i f_i(t_i) + (1 -
\delta_i) S_i(t_i)\]\$\$

where \\\delta_i\\ is the censoring indicator, \\f_i\\ the predicted
probability density function and \\S_i\\ the predicted survival function
for observation \\i\\. RCLL is proper given that censoring and survival
distribution are independent, see Rindt et al. (2022). Simulation
studies by Sonabend et al. (2024) provide strong empirical evidence
supporting the properness of this score. See section **Interpolation**
for implementation details.

To get a single score across all \\N\\ observations of the test set, we
return the average of the observation-wise scores:

\$\$\sum\_{i=1}^N L\_{RCLL}(S_i, t_i, \delta_i) / N\$\$

## Dictionary

This [Measure](https://mlr3.mlr-org.com/reference/Measure.html) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_measures](https://mlr3.mlr-org.com/reference/mlr_measures.html) or
with the associated sugar function
[msr()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    MeasureSurvRCLL$new()
    mlr_measures$get("surv.rcll")
    msr("surv.rcll")

## Parameters

|     |         |         |             |              |
|-----|---------|---------|-------------|--------------|
| Id  | Type    | Default | Levels      | Range        |
| eps | numeric | 1e-06   |             | \\\[0, 1\]\\ |
| ERV | logical | FALSE   | TRUE, FALSE | \-           |

## Meta Information

- Type: `"surv"`

- Range: \\\[0, \infty)\\

- Minimize: `TRUE`

- Required prediction: `distr`

## Parameter details

- `eps` (`numeric(1)`)  
  Very small number to substitute near-zero values in order to prevent
  errors in e.g. log(0) and/or division-by-zero calculations. Default
  value is 1e-06.

&nbsp;

- `ERV` (`logical(1)`)  
  If `TRUE` then the Explained Residual Variation method is applied,
  which means the score is standardized against a Kaplan-Meier baseline.
  Default is `FALSE`.

## Interpolation

To evaluate scores involving subject-specific survival functions
\\S_i(t)\\, we perform **linear interpolation** on the discrete survival
values provided in the prediction. Duplicate survival values are removed
prior to interpolation to ensure strict monotonicity and non-negative
density values. Therefore we are left with the distinct survival time
points \\t_0 \< \cdots \< t_n\\ and the corresponding survival values
\\S(t_j)\\.

Interpolation is performed using base R’s
[`approx()`](https://rdrr.io/r/stats/approxfun.html) with
`method = "linear"` and `rule = 2`, ensuring:

- **Left extrapolation** (for \\t \< t_0\\) assumes \\S(0) = 1\\ and
  uses the slope from \\(0, 1)\\ to \\(t_0, S(t_0))\\.

- **Right extrapolation** (for \\t \> t_n\\) uses the slope from the
  last interval \\(t\_{n-1}, S(t\_{n-1}))\\ to \\(t_n, S(t_n))\\, with
  results truncated at 0 to preserve non-negativity.

This ensures a continuous, piecewise-linear survival function \\S(t)\\
that satisfies \\S(0) = 1\\ and remains non-increasing and non-negative
across the entire domain.

The density at time point \\t_k\\, with \\t_i \le t_k \< t\_{i+1}\\, is
estimated as follows:

\$\$ f_i(t_k) = -\frac{S_i(t\_{i+1}) - S_i(t_i)}{t\_{i+1} - t_i} \$\$

This corresponds to the (negative) slope of the \\S_i(t)\\ between the
closest grid point after \\t_i\\ and \\t_i\\ itself.

## References

Avati, Anand, Duan, Tony, Zhou, Sharon, Jung, Kenneth, Shah, H N, Ng, Y
A (2020). “Countdown Regression: Sharp and Calibrated Survival
Predictions.” *Proceedings of The 35th Uncertainty in Artificial
Intelligence Conference*, **115**(4), 145–155.
<https://proceedings.mlr.press/v115/avati20a.html>.

Rindt, David, Hu, Robert, Steinsaltz, David, Sejdinovic, Dino (2022).
“Survival regression with proper scoring rules and monotonic neural
networks.” *Proceedings of The 25th International Conference on
Artificial Intelligence and Statistics*, **151**(4), 1190–1205.
<https://proceedings.mlr.press/v151/rindt22a.html>.

Sonabend, Raphael, Zobolas, John, Kopper, Philipp, Burk, Lukas, Bender,
Andreas (2024). “Examining properness in the external validation of
survival models with squared and logarithmic losses.”
<https://arxiv.org/abs/2212.05260v3>.

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
[`mlr_measures_surv.rmse`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.rmse.md),
[`mlr_measures_surv.schmid`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.schmid.md),
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
[`mlr_measures_surv.schmid`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.schmid.md)

Other distr survival measures:
[`mlr_measures_surv.calib_alpha`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_alpha.md),
[`mlr_measures_surv.calib_index`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.calib_index.md),
[`mlr_measures_surv.dcalib`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.dcalib.md),
[`mlr_measures_surv.graf`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.graf.md),
[`mlr_measures_surv.intlogloss`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.intlogloss.md),
[`mlr_measures_surv.logloss`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.logloss.md),
[`mlr_measures_surv.schmid`](https://mlr3proba.mlr-org.com/reference/mlr_measures_surv.schmid.md)

## Super classes

[`mlr3::Measure`](https://mlr3.mlr-org.com/reference/Measure.html) -\>
[`mlr3proba::MeasureSurv`](https://mlr3proba.mlr-org.com/reference/MeasureSurv.md)
-\> `MeasureSurvRCLL`

## Methods

### Public methods

- [`MeasureSurvRCLL$new()`](#method-MeasureSurvRCLL-new)

- [`MeasureSurvRCLL$clone()`](#method-MeasureSurvRCLL-clone)

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

    MeasureSurvRCLL$new(ERV = FALSE)

#### Arguments

- `ERV`:

  (`logical(1)`)  
  Standardize measure against a Kaplan-Meier baseline (Explained
  Residual Variation)

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    MeasureSurvRCLL$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
