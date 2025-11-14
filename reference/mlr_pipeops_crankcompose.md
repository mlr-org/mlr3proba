# PipeOpCrankCompositor

Uses a predicted `distr` in a
[PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md)
to estimate (or 'compose') a `crank` prediction.

## Dictionary

This [PipeOp](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr3pipelines::mlr_pipeops](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops.html)
or with the associated sugar function
[`mlr3pipelines::po()`](https://mlr3pipelines.mlr-org.com/reference/po.html):

    PipeOpCrankCompositor$new()
    mlr_pipeops$get("crankcompose")
    po("crankcompose")

## Input and Output Channels

PipeOpCrankCompositor has one input channel named `"input"`, which takes
`NULL` during training and
[PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md)
during prediction.

PipeOpCrankCompositor has one output channel named `"output"`, producing
`NULL` during training and a
[PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md)
during prediction.

The output during prediction is the
[PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md)
from the input but with the `crank` predict type overwritten by the
given estimation method.

## State

The `$state` is left empty
([`list()`](https://rdrr.io/r/base/list.html)).

## Parameters

- `method` :: `character(1)`  
  Determines what method should be used to produce a continuous ranking
  from the distribution. Currently only `mort` is supported, which is
  the sum of the cumulative hazard, also called *expected/ensemble
  mortality*, see Ishwaran et al. (2008). For more details, see
  [`get_mortality()`](https://mlr3proba.mlr-org.com/reference/get_mortality.md).

- `overwrite` :: `logical(1)`  
  If `FALSE` (default) and the prediction already has a `crank`
  prediction, then the compositor returns the input prediction
  unchanged. If `TRUE`, then the `crank` will be overwritten.

## References

Sonabend, Raphael, Bender, Andreas, Vollmer, Sebastian (2022). “Avoiding
C-hacking when evaluating survival distribution predictions with
discrimination measures.” *Bioinformatics*. ISSN 1367-4803,
[doi:10.1093/BIOINFORMATICS/BTAC451](https://doi.org/10.1093/BIOINFORMATICS/BTAC451)
,
<https://academic.oup.com/bioinformatics/advance-article/doi/10.1093/bioinformatics/btac451/6640155>.

Ishwaran, Hemant, Kogalur, B U, Blackstone, H E, Lauer, S M, others
(2008). “Random survival forests.” *The Annals of applied statistics*,
**2**(3), 841–860.

## See also

[pipeline_crankcompositor](https://mlr3proba.mlr-org.com/reference/mlr_graphs_crankcompositor.md)

Other survival compositors:
[`mlr_pipeops_compose_breslow_distr`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_compose_breslow_distr.md),
[`mlr_pipeops_distrcompose`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_distrcompose.md),
[`mlr_pipeops_responsecompose`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_responsecompose.md)

## Super class

[`mlr3pipelines::PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
-\> `PipeOpCrankCompositor`

## Methods

### Public methods

- [`PipeOpCrankCompositor$new()`](#method-PipeOpCrankCompositor-new)

- [`PipeOpCrankCompositor$clone()`](#method-PipeOpCrankCompositor-clone)

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

    PipeOpCrankCompositor$new(id = "crankcompose", param_vals = list())

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

    PipeOpCrankCompositor$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
  library(mlr3pipelines)
  task = tsk("rats")

  # change the crank prediction type of a Cox's model predictions
  pred = lrn("surv.coxph")$train(task)$predict(task)
  poc = po("crankcompose", param_vals = list(overwrite = TRUE))
  poc$train(list(NULL)) # need to train first, even if nothing happens
  poc$predict(list(pred))[[1L]]
} # }
```
