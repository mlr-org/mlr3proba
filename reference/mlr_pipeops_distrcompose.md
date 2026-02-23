# PipeOpDistrCompositor

**\[experimental\]**

Estimates (or 'composes') a survival distribution from a predicted
baseline survival distribution (`distr`) and a linear predictor (`lp`)
from two
[PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md)s.

Compositor Assumptions:

- The baseline `distr` is a discrete estimator, e.g.
  [surv.kaplan](https://mlr3proba.mlr-org.com/reference/mlr_learners_surv.kaplan.md).

- The composed `distr` is of a linear form

## Dictionary

This [PipeOp](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr3pipelines::mlr_pipeops](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops.html)
or with the associated sugar function
[`mlr3pipelines::po()`](https://mlr3pipelines.mlr-org.com/reference/po.html):

    PipeOpDistrCompositor$new()
    mlr_pipeops$get("distrcompose")
    po("distrcompose")

## Input and Output Channels

PipeOpDistrCompositor has two input channels, `"base"` and `"pred"`.
Both input channels take `NULL` during training and
[PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md)
during prediction.

PipeOpDistrCompositor has one output channel named `"output"`, producing
`NULL` during training and a
[PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md)
during prediction.

The output during prediction is the
[PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md)
from the `"pred"` input but with an extra (or overwritten) column for
the `distr` predict type; which is composed from the `distr` of `"base"`
and the `lp` of `"pred"`. If no `lp` predictions have been made or
exist, then the `"pred"` is returned unchanged.

## State

The `$state` is left empty
([`list()`](https://rdrr.io/r/base/list.html)).

## Parameters

The parameters are:

- `form` :: `character(1)`  
  Determines the form that the predicted linear survival model should
  take. This is either, accelerated-failure time, `aft`, proportional
  hazards, `ph`, or proportional odds, `po`. Default `aft`.

- `overwrite` :: `logical(1)`  
  If `FALSE` (default) then if the "pred" input already has a `distr`,
  the compositor does nothing and returns the given
  [PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md).
  If `TRUE`, then the `distr` is overwritten with the `distr` composed
  from `lp` - this is useful for changing the prediction `distr` from
  one model form to another.

- `scale_lp` :: `logical(1)`  
  This option is only applicable to `form` equal to `"aft"`. If `TRUE`,
  it min-max scales the linear prediction scores to be in the interval
  \\\[0,1\]\\, avoiding extrapolation of the baseline \\S_0(t)\\ on the
  transformed time points \\\frac{t}{\exp(lp)}\\, as these will be \\\in
  \[\frac{t}{e}, t\]\\, and so always smaller than the maximum time
  point for which we have estimated \\S_0(t)\\. Note that this is just a
  **heuristic** to get reasonable results in the case you observe
  survival predictions to be e.g. constant after the AFT composition and
  it definitely provides no guarantee for creating calibrated
  distribution predictions (as none of these methods do). Therefore, it
  is set to `FALSE` by default.

## Internals

The respective `form`s above have respective survival distributions:
\$\$aft: S(t) = S_0(\frac{t}{\exp(lp)})\$\$ \$\$ph: S(t) =
S_0(t)^{\exp(lp)}\$\$ \$\$po: S(t) = \frac{S_0(t)}{\exp(-lp) +
(1-\exp(-lp)) S_0(t)}\$\$ where \\S_0\\ is the estimated baseline
survival distribution, and \\lp\\ is the predicted linear predictor.

For an example use of the `"aft"` composition using Kaplan-Meier as a
baseline distribution, see Norman et al. (2024).

## References

Norman, A P, Li, Wanlu, Jiang, Wenyu, Chen, E B (2024). “deepAFT: A
nonlinear accelerated failure time model with artificial neural
network.” *Statistics in Medicine*.
[doi:10.1002/sim.10152](https://doi.org/10.1002/sim.10152) .

## See also

[pipeline_distrcompositor](https://mlr3proba.mlr-org.com/reference/mlr_graphs_distrcompositor.md)

Other survival compositors:
[`mlr_pipeops_compose_breslow_distr`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_compose_breslow_distr.md),
[`mlr_pipeops_crankcompose`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_crankcompose.md),
[`mlr_pipeops_responsecompose`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_responsecompose.md)

## Super class

[`mlr3pipelines::PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
-\> `PipeOpDistrCompositor`

## Methods

### Public methods

- [`PipeOpDistrCompositor$new()`](#method-PipeOpDistrCompositor-new)

- [`PipeOpDistrCompositor$clone()`](#method-PipeOpDistrCompositor-clone)

Inherited methods

- [`mlr3pipelines::PipeOp$help()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-help)
- [`mlr3pipelines::PipeOp$predict()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-predict)
- [`mlr3pipelines::PipeOp$print()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-print)
- [`mlr3pipelines::PipeOp$train()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-train)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://r6.r-lib.org/reference/R6Class.html) class.

#### Usage

    PipeOpDistrCompositor$new(id = "distrcompose", param_vals = list())

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier of the resulting object.

- `param_vals`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PipeOpDistrCompositor$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
  library(mlr3)
  library(mlr3pipelines)
  task = tsk("rats")

  base = lrn("surv.kaplan")$train(task)$predict(task)
  pred = lrn("surv.coxph")$train(task)$predict(task)
  # let's change the distribution prediction of Cox (Breslow-based) to an AFT form:
  pod = po("distrcompose", param_vals = list(form = "aft", overwrite = TRUE))
  pod$train(list(NULL, NULL)) # need to train first, even if nothing happens
  pod$predict(list(base = base, pred = pred))[[1]]
} # }
```
