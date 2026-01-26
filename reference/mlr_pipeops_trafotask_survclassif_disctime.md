# PipeOpTaskSurvClassifDiscTime

Transform
[TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md) to
[TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html) by
dividing continuous time into multiple time intervals for each
observation. This transformation creates a new target variable
`disc_status` that indicates whether an event occurred within each time
interval. This approach facilitates survival analysis within a
classification framework using discrete time intervals (Tutz et al.
2016).

Note that this data transformation is compatible with learners that
support the `"validation"` property and can track performance on holdout
data during training, enabling early stopping and logging.

## Dictionary

This [PipeOp](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr3pipelines::mlr_pipeops](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops.html)
or with the associated sugar function
[`mlr3pipelines::po()`](https://mlr3pipelines.mlr-org.com/reference/po.html):

    PipeOpTaskSurvClassifDiscTime$new()
    mlr_pipeops$get("trafotask_survclassif_disctime")
    po("trafotask_survclassif_disctime")

## Input and Output Channels

PipeOpTaskSurvClassifDiscTime has one input channel named "input", and
two output channels, one named "output" and the other
"transformed_data".

During training, the "output" is the "input"
[TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md)
transformed to a
[TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html). The
target column is named `"disc_status"` and indicates whether an event
occurred in each time interval. An additional numeric feature named
`"tend"` contains the end time point of each interval. Lastly, the
"output" task has a column with the original observation ids, under the
role `"original_ids"`. The "transformed_data" is an empty
[data.table](https://rdrr.io/pkg/data.table/man/data.table.html).

During prediction, the "input"
[TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md) is
transformed to the "output"
[TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html) with
`"disc_status"` as target and the `"tend"` feature included. The
"transformed_data" is a
[data.table](https://rdrr.io/pkg/data.table/man/data.table.html) with
columns the `"disc_status"` target of the "output" task, the `"id"`
(original observation ids), `"obs_times"` (observed times per `"id"`)
and `"tend"` (end time of each interval). This "transformed_data" is
only meant to be used with the
[PipeOpPredClassifSurvDiscTime](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_classifsurv_disctime.md).

## State

The `$state` contains information about the `cut` parameter used.

## Parameters

The parameters are

- `cut :: numeric()`  
  Split points, used to partition the data into intervals based on the
  `time` column. If unspecified, all unique event times will be used. If
  `cut` is a single integer, it will be interpreted as the number of
  equidistant intervals from 0 until the maximum event time.

- `max_time :: numeric(1)`  
  If `cut` is unspecified, this will be the last possible event time.
  All event times after `max_time` will be administratively censored at
  `max_time.` Needs to be greater than the minimum event time in the
  given task.

## References

Tutz, Gerhard, Schmid, Matthias (2016). *Modeling Discrete Time-to-Event
Data*, series Springer Series in Statistics. Springer International
Publishing. ISBN 978-3-319-28156-8 978-3-319-28158-2,
<http://link.springer.com/10.1007/978-3-319-28158-2>.

## See also

[pipeline_survtoclassif_disctime](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survtoclassif_disctime.md)

Other Transformation PipeOps:
[`mlr_pipeops_trafopred_classifsurv_IPCW`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_classifsurv_IPCW.md),
[`mlr_pipeops_trafopred_classifsurv_disctime`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_classifsurv_disctime.md),
[`mlr_pipeops_trafopred_regrsurv_pem`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_regrsurv_pem.md),
[`mlr_pipeops_trafotask_survclassif_IPCW`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafotask_survclassif_IPCW.md),
[`mlr_pipeops_trafotask_survregr_pem`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafotask_survregr_pem.md)

## Super class

[`mlr3pipelines::PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
-\> `PipeOpTaskSurvClassifDiscTime`

## Methods

### Public methods

- [`PipeOpTaskSurvClassifDiscTime$new()`](#method-PipeOpTaskSurvClassifDiscTime-new)

- [`PipeOpTaskSurvClassifDiscTime$clone()`](#method-PipeOpTaskSurvClassifDiscTime-clone)

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

    PipeOpTaskSurvClassifDiscTime$new(id = "trafotask_survclassif_disctime")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier of the resulting object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PipeOpTaskSurvClassifDiscTime$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # \dontrun{
  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)

  task = tsk("lung")

  # transform the survival task to a classification task
  # all unique event times are used as cutpoints
  po_disc = po("trafotask_survclassif_disctime")
  task_classif = po_disc$train(list(task))[[1L]]

  # the end time points of the discrete time intervals
  unique(task_classif$data(cols = "tend"))[[1L]]

  # train a classification learner
  learner = lrn("classif.log_reg", predict_type = "prob")
  learner$train(task_classif)
} # }
```
