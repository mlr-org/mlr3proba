# PipeOpResponseCompositor

Uses a predicted survival distribution (`distr`) in a
[PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md)
to estimate (or 'compose') an expected survival time (`response`)
prediction. Practically, this `PipeOp` summarizes an observation's
survival curve/distribution to a single number which can be either the
restricted mean survival time or the median survival time.

## Dictionary

This [PipeOp](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr3pipelines::mlr_pipeops](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops.html)
or with the associated sugar function
[`mlr3pipelines::po()`](https://mlr3pipelines.mlr-org.com/reference/po.html):

    PipeOpResponseCompositor$new()
    mlr_pipeops$get("responsecompose")
    po("responsecompose")

## Input and Output Channels

PipeOpResponseCompositor has one input channel named `"input"`, which
takes `NULL` during training and
[PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md)
during prediction.

PipeOpResponseCompositor has one output channel named `"output"`,
producing `NULL` during training and a
[PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md)
during prediction.

The output during prediction is the
[PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md)
from the input but with the `response` predict type overwritten by the
given method.

## State

The `$state` is left empty
([`list()`](https://rdrr.io/r/base/list.html)).

## Parameters

- `method` :: `character(1)`  
  Determines what method should be used to produce a survival time
  (response) from the survival distribution. Available methods are
  `"rmst"` and `"median"`, corresponding to the *restricted mean
  survival time* and the *median survival time* respectively.

- `tau` :: `numeric(1)`  
  Determines the time point up to which we calculate the restricted mean
  survival time (works only for the `"rmst"` method). If `NULL`
  (default), all the available time points in the predicted survival
  distribution will be used.

&nbsp;

- `add_crank` :: `logical(1)`  
  If `TRUE` then `crank` predict type will be set as `-response` (as
  higher survival times correspond to lower risk). Works only if
  `overwrite` is `TRUE`.

- `overwrite` :: `logical(1)`  
  If `FALSE` (default) and the prediction already has a `response`
  prediction, then the compositor returns the input prediction
  unchanged. If `TRUE`, then the `response` (and the `crank`, if
  `add_crank` is `TRUE`) will be overwritten.

## Internals

The restricted mean survival time is the default/preferred method and is
calculated as follows: \$\$T\_{i,rmst} \approx \sum\_{t_j \in
\[0,\tau\]} (t_j - t\_{j-1}) S_i(t_j)\$\$

where \\T\\ is the expected survival time, \\\tau\\ is the time
cutoff/horizon and \\S_i(t_j)\\ are the predicted survival probabilities
of observation \\i\\ for all the \\t_j\\ time points.

The \\T\_{i,median}\\ survival time is just the first time point for
which the survival probability is less than \\0.5\\. If no such time
point exists (e.g. when the survival distribution is not proper due to
high censoring) we return the last time point. This is **not a good
estimate to use in general**, only a reasonable substitute in such
cases.

## References

Zhao, Lihui, Claggett, Brian, Tian, Lu, Uno, Hajime, Pfeffer, A. M,
Solomon, D. S, Trippa, Lorenzo, Wei, J. L (2016). “On the restricted
mean survival time curve in survival analysis.” *Biometrics*, **72**(1),
215–221. ISSN 1541-0420,
[doi:10.1111/BIOM.12384](https://doi.org/10.1111/BIOM.12384) ,
<https://onlinelibrary.wiley.com/doi/full/10.1111/biom.12384>.

## See also

[pipeline_responsecompositor](https://mlr3proba.mlr-org.com/reference/mlr_graphs_responsecompositor.md)

Other survival compositors:
[`mlr_pipeops_compose_breslow_distr`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_compose_breslow_distr.md),
[`mlr_pipeops_crankcompose`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_crankcompose.md),
[`mlr_pipeops_distrcompose`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_distrcompose.md)

## Super class

[`mlr3pipelines::PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
-\> `PipeOpResponseCompositor`

## Methods

### Public methods

- [`PipeOpResponseCompositor$new()`](#method-PipeOpResponseCompositor-new)

- [`PipeOpResponseCompositor$clone()`](#method-PipeOpResponseCompositor-clone)

Inherited methods

- [`mlr3pipelines::PipeOp$help()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-help)
- [`mlr3pipelines::PipeOp$predict()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-predict)
- [`mlr3pipelines::PipeOp$print()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-print)
- [`mlr3pipelines::PipeOp$train()`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html#method-train)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://rdrr.io/pkg/R6/man/R6Class.html) class.

#### Usage

    PipeOpResponseCompositor$new(id = "responsecompose", param_vals = list())

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

    PipeOpResponseCompositor$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
  library(mlr3pipelines)
  task = tsk("rats")

  # add survival time prediction type to the predictions of a Cox model
  # Median survival time as response
  pred = lrn("surv.coxph")$train(task)$predict(task)
  por = po("responsecompose", param_vals = list(method = "median", overwrite = TRUE))
  por$train(list(NULL)) # need to train first, even if nothing happens
  por$predict(list(pred))[[1L]]
  # mostly improper survival distributions, "median" sets the survival time
  # to the last time point

  # RMST (default) as response, while also changing the `crank` to `-response`
  por = po("responsecompose", param_vals = list(overwrite = TRUE, add_crank = TRUE))
  por$train(list(NULL))
  por$predict(list(pred))[[1L]]
} # }
```
