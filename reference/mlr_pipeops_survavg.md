# PipeOpSurvAvg

Perform (weighted) prediction averaging from survival
[PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md)s
by connecting `PipeOpSurvAvg` to multiple
[PipeOpLearner](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops_learner.html)
outputs.

The resulting prediction will aggregate any predict types that are
contained within all inputs. Any predict types missing from at least one
input will be set to `NULL`. These are aggregated as follows:

- `"response"`, `"crank"`, and `"lp"` are all a weighted average from
  the incoming predictions.

- `"distr"` is a
  [distr6::VectorDistribution](https://xoopr.github.io/distr6/reference/VectorDistribution.html)
  containing
  [distr6::MixtureDistribution](https://xoopr.github.io/distr6/reference/MixtureDistribution.html)s.

Weights can be set as a parameter; if none are provided, defaults to
equal weights for each prediction.

## Input and Output Channels

Input and output channels are inherited from
[PipeOpEnsemble](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.html)
with a
[PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md)
for inputs and outputs.

## State

The `$state` is left empty
([`list()`](https://rdrr.io/r/base/list.html)).

## Parameters

The parameters are the parameters inherited from the
[PipeOpEnsemble](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.html).

## Internals

Inherits from
[PipeOpEnsemble](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.html)
by implementing the `private$weighted_avg_predictions()` method.

## See also

[pipeline_survaverager](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survaverager.md)

Other PipeOps:
[`mlr_pipeops_trafopred_regrsurv_pem`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_regrsurv_pem.md),
[`mlr_pipeops_trafotask_survregr_pem`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafotask_survregr_pem.md)

## Super classes

[`mlr3pipelines::PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
-\>
[`mlr3pipelines::PipeOpEnsemble`](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.html)
-\> `PipeOpSurvAvg`

## Methods

### Public methods

- [`PipeOpSurvAvg$new()`](#method-PipeOpSurvAvg-new)

- [`PipeOpSurvAvg$clone()`](#method-PipeOpSurvAvg-clone)

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

    PipeOpSurvAvg$new(innum = 0, id = "survavg", param_vals = list(), ...)

#### Arguments

- `innum`:

  (`numeric(1)`)  
  Determines the number of input channels. If `innum` is 0 (default), a
  vararg input channel is created that can take an arbitrary number of
  inputs.

- `id`:

  (`character(1)`)  
  Identifier of the resulting object.

- `param_vals`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction.

- `...`:

  (`ANY`)  
  Additional arguments passed to
  [mlr3pipelines::PipeOpEnsemble](https://mlr3pipelines.mlr-org.com/reference/PipeOpEnsemble.html).

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PipeOpSurvAvg$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
  library(mlr3)
  library(mlr3pipelines)

  task = tsk("rats")
  p1 = lrn("surv.coxph")$train(task)$predict(task)
  p2 = lrn("surv.kaplan")$train(task)$predict(task)
  poc = po("survavg", param_vals = list(weights = c(0.2, 0.8)))
  poc$train(list(NULL)) # need to train first, even if nothing happens
  poc$predict(list(p1, p2))
} # }
```
