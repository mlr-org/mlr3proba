# PipeOpProbregr

**\[experimental\]**

Combines a predicted `response` and `se` from
[PredictionRegr](https://mlr3.mlr-org.com/reference/PredictionRegr.html)
with a specified probability distribution to estimate (or 'compose') a
`distr` prediction.

## Dictionary

This [PipeOp](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr3pipelines::mlr_pipeops](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops.html)
or with the associated sugar function
[`mlr3pipelines::po()`](https://mlr3pipelines.mlr-org.com/reference/po.html):

    PipeOpProbregr$new()
    mlr_pipeops$get("compose_probregr")
    po("compose_probregr")

## Input and Output Channels

PipeOpProbregr has two input channels named `"input_response"` and
`"input_se"`, which take `NULL` during training and two
[PredictionRegr](https://mlr3.mlr-org.com/reference/PredictionRegr.html)s
during prediction, these should respectively contain the `response` and
`se` return type, the same object can be passed twice.

The output during prediction is a
[PredictionRegr](https://mlr3.mlr-org.com/reference/PredictionRegr.html)
with the "response" from `input_response`, the "se" from `input_se` and
a "distr" created from combining the two.

## State

The `$state` is left empty
([`list()`](https://rdrr.io/r/base/list.html)).

## Parameters

- `dist` :: `character(1)`  
  Location-scale distribution to use for composition. Current choices
  are `"Uniform"` (default), `"Normal"`, `"Cauchy"`, `"Gumbel"`,
  `"Laplace"`, `"Logistic"`. All implemented via
  [distr6](https://xoopr.github.io/distr6/reference/distr6-package.html).

## Internals

The composition is created by substituting the `response` and `se`
predictions into the distribution location and scale parameters
respectively.

## Super class

[`mlr3pipelines::PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
-\> `PipeOpProbregr`

## Methods

### Public methods

- [`PipeOpProbregr$new()`](#method-PipeOpProbregr-new)

- [`PipeOpProbregr$clone()`](#method-PipeOpProbregr-clone)

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

    PipeOpProbregr$new(id = "compose_probregr", param_vals = list())

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

    PipeOpProbregr$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
  library(mlr3)
  library(mlr3pipelines)
  set.seed(1)
  task = tsk("boston_housing")

  # Option 1: Use a learner that can predict se
  learn = lrn("regr.featureless", predict_type = "se")
  pred = learn$train(task)$predict(task)
  poc = po("compose_probregr")
  poc$train(list(NULL, NULL))
  poc$predict(list(pred, pred))[[1]]

  # Option 2: Use two learners, one for response and the other for se
  learn_response = lrn("regr.rpart")
  learn_se = lrn("regr.featureless", predict_type = "se")
  pred_response = learn_response$train(task)$predict(task)
  pred_se = learn_se$train(task)$predict(task)
  poc = po("compose_probregr")
  poc$train(list(NULL, NULL))
  poc$predict(list(pred_response, pred_se))[[1]]
} # }
```
