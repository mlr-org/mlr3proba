# Wrap a learner into a PipeOp with survival predictions estimated by the Breslow estimator

Composes a survival distribution (`distr`) using the linear predictor
predictions (`lp`) from a given
[LearnerSurv](https://mlr3proba.mlr-org.com/reference/LearnerSurv.md)
during training and prediction, utilizing the [breslow
estimator](https://mlr3proba.mlr-org.com/reference/breslow.md). The
specified `learner` must be capable of generating `lp`-type predictions
(e.g., a Cox-type model).

## Dictionary

This [PipeOp](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
can be instantiated via the
[Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_pipeops](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops.html)
or with the associated sugar function
[po()](https://mlr3pipelines.mlr-org.com/reference/po.html):

    PipeOpBreslow$new(learner)
    mlr_pipeops$get("breslowcompose", learner)
    po("breslowcompose", learner, breslow.overwrite = TRUE)

## Input and Output Channels

PipeOpBreslow is like a
[LearnerSurv](https://mlr3proba.mlr-org.com/reference/LearnerSurv.md).
It has one input channel, named `input` that takes a
[TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md) during
training and another
[TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md) during
prediction. PipeOpBreslow has one output channel named `output`,
producing `NULL` during training and a
[PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md)
during prediction.

## State

The `$state` slot stores the `times` and `status` survival target
variables of the train
[TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md) as well
as the `lp` predictions on the train set.

## Parameters

The parameters are:

- `breslow.overwrite` :: `logical(1)`  
  If `FALSE` (default) then the compositor does nothing and returns the
  input `learner`'s
  [PredictionSurv](https://mlr3proba.mlr-org.com/reference/PredictionSurv.md).
  If `TRUE` or in the case that the input `learner` doesn't have `distr`
  predictions, then the `distr` is overwritten with the `distr` composed
  from `lp` and the train set information using
  [breslow](https://mlr3proba.mlr-org.com/reference/breslow.md). This is
  useful for changing the prediction `distr` from one model form to
  another.

## References

Breslow N (1972). “Discussion of 'Regression Models and Life-Tables' by
D.R. Cox.” *Journal of the Royal Statistical Society: Series B*,
**34**(2), 216-217.

Lin, Y. D (2007). “On the Breslow estimator.” *Lifetime Data Analysis*,
**13**(4), 471-480.
[doi:10.1007/s10985-007-9048-y](https://doi.org/10.1007/s10985-007-9048-y)
.

## See also

[pipeline_distrcompositor](https://mlr3proba.mlr-org.com/reference/mlr_graphs_distrcompositor.md)

Other survival compositors:
[`mlr_pipeops_crankcompose`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_crankcompose.md),
[`mlr_pipeops_distrcompose`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_distrcompose.md),
[`mlr_pipeops_responsecompose`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_responsecompose.md)

## Super class

[`mlr3pipelines::PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
-\> `PipeOpBreslow`

## Active bindings

- `learner`:

  ([mlr3::Learner](https://mlr3.mlr-org.com/reference/Learner.html))  
  The input survival learner.

## Methods

### Public methods

- [`PipeOpBreslow$new()`](#method-PipeOpBreslow-new)

- [`PipeOpBreslow$clone()`](#method-PipeOpBreslow-clone)

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

    PipeOpBreslow$new(learner, id = NULL, param_vals = list())

#### Arguments

- `learner`:

  ([LearnerSurv](https://mlr3proba.mlr-org.com/reference/LearnerSurv.md))  
  Survival learner which must provide `lp`-type predictions

- `id`:

  (character(1))  
  Identifier of the resulting object. If `NULL` (default), it will be
  set as the `id` of the input `learner`.

- `param_vals`:

  ([`list()`](https://rdrr.io/r/base/list.html))  
  List of hyperparameter settings, overwriting the hyperparameter
  settings that would otherwise be set during construction.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PipeOpBreslow$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
  library(mlr3)
  library(mlr3pipelines)
  task = tsk("rats")
  part = partition(task, ratio = 0.8)
  train_task = task$clone()$filter(part$train)
  test_task = task$clone()$filter(part$test)

  learner = lrn("surv.coxph") # learner with lp predictions
  b = po("breslowcompose", learner = learner, breslow.overwrite = TRUE)

  b$train(list(train_task))
  p = b$predict(list(test_task))[[1L]]
} # }
```
