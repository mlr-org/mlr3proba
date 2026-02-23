# PipeOpTaskSurvRegrPEM

Transform
[TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md) to
[TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html) by dividing
continuous time into multiple time intervals for each observation. This
transformation creates a new target variable `pem_status` that indicates
whether an event occurred within each time interval. The piece-wise
exponential modeling approach (PEM) facilitates survival analysis within
a regression framework using discrete time intervals (Bender et al.
2018).

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

    PipeOpTaskSurvRegrPEM$new()
    mlr_pipeops$get("trafotask_survregr_pem")
    po("trafotask_survregr_pem")

## Input and Output Channels

PipeOpTaskSurvRegrPEM has one input channel named "input", and two
output channels, one named "output" and the other "transformed_data".

During training, the "output" is the "input"
[TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md)
transformed to a
[TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html). The target
column is named `"pem_status"` and indicates whether an event occurred
in each time interval. An additional numeric feature named `"tend"`
contains the end time point of each interval. Lastly, the "output" task
has an offset column `"offset"`. The offset, also referred to as
*exposure*, is the **logarithm of time spent in interval** \\j\\, i.e.
\\log(t_j)\\. The "transformed_data" is an empty
[data.table](https://rdrr.io/pkg/data.table/man/data.table.html).

During prediction, the "input"
[TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md) is
transformed to the "output"
[TaskRegr](https://mlr3.mlr-org.com/reference/TaskRegr.html) with
`"pem_status"` as target, `"tend"` included as feature and and the
`"offset"` column which is assigned the offset `"col_role"`. The
"transformed_data" is a
[data.table](https://rdrr.io/pkg/data.table/man/data.table.html) with
columns the `"pem_status"` target of the "output" task, the `"id"`
(original observation ids), `"obs_times"` (observed times per `"id"`)
and `"tend"` (end time of each interval). This "transformed_data" is
only meant to be used with the
[PipeOpPredRegrSurvPEM](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_regrsurv_pem.md).

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

Bender, Andreas, Groll, Andreas, Scheipl, Fabian (2018). “A generalized
additive model approach to time-to-event analysis.” *Statistical
Modelling*, **18**(3-4), 299–321.
<https://doi.org/10.1177/1471082X17748083>.

## See also

[pipeline_survtoregr_pem](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survtoregr_pem.md)

Other PipeOps:
[`mlr_pipeops_survavg`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_survavg.md),
[`mlr_pipeops_trafopred_regrsurv_pem`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_regrsurv_pem.md)

Other Transformation PipeOps:
[`mlr_pipeops_trafopred_classifsurv_IPCW`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_classifsurv_IPCW.md),
[`mlr_pipeops_trafopred_classifsurv_disctime`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_classifsurv_disctime.md),
[`mlr_pipeops_trafopred_regrsurv_pem`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_regrsurv_pem.md),
[`mlr_pipeops_trafotask_survclassif_IPCW`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafotask_survclassif_IPCW.md),
[`mlr_pipeops_trafotask_survclassif_disctime`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafotask_survclassif_disctime.md)

## Super class

[`mlr3pipelines::PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
-\> `PipeOpTaskSurvRegrPEM`

## Methods

### Public methods

- [`PipeOpTaskSurvRegrPEM$new()`](#method-PipeOpTaskSurvRegrPEM-new)

- [`PipeOpTaskSurvRegrPEM$clone()`](#method-PipeOpTaskSurvRegrPEM-clone)

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

    PipeOpTaskSurvRegrPEM$new(id = "trafotask_survregr_pem")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier of the resulting object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PipeOpTaskSurvRegrPEM$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
if (FALSE) { # (mlr3misc::require_namespaces(c("mlr3pipelines", "mlr3extralearners"), quietly = TRUE))
if (FALSE) { # \dontrun{
  library(mlr3)
  library(mlr3learners)
  library(mlr3pipelines)

  task = tsk("lung")

  # transform the survival task to a regression task
  # all unique event times are used as cutpoints
  po_pem = po("trafotask_survregr_pem")
  task_regr = po_pem$train(list(task))[[1L]]

  # the end time points of the discrete time intervals
  unique(task_regr$data(cols = "tend")[[1L]])

  # train a regression learner that supports poisson regression
  # e.g. regr.gam
  # won't run unless learner can accept offset column role
  learner = lrn("regr.gam", formula = pem_status ~ s(age) + s(tend), family = "poisson")
  learner$train(task_regr)

  # e.g. regr.xgboost, note prior data processing steps
  learner = as_learner(
    po("modelmatrix", formula = ~ as.factor(tend) + .) %>>%
    lrn("regr.xgboost", objective = "count:poisson", nrounds = 100, eta = 0.1)
  )
  learner$train(task_regr)
  } # }
}
```
