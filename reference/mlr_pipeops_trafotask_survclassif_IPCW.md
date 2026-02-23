# PipeOpTaskSurvClassifIPCW

Transform
[TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md) to
[TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html) using
the **I**nverse **P**robability of **C**ensoring **W**eights (IPCW)
method by Vock et al. (2016).

Let \\T_i\\ be the observed times (event or censoring) and \\\delta_i\\
the censoring indicators for each observation \\i\\ in the training set.
The IPCW technique consists of two steps: first we estimate the
censoring distribution \\\hat{G}(t)\\ using the Kaplan-Meier estimator
from the training data. Then we calculate the observation weights given
a cutoff time \\\tau\\ as:

\$\$\omega_i = 1/\hat{G}{(min(T_i,\tau))}\$\$

Observations that are censored prior to \\\tau\\ are assigned zero
weights, i.e. \\\omega_i = 0\\.

## Dictionary

This [PipeOp](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
can be instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr3pipelines::mlr_pipeops](https://mlr3pipelines.mlr-org.com/reference/mlr_pipeops.html)
or with the associated sugar function
[`mlr3pipelines::po()`](https://mlr3pipelines.mlr-org.com/reference/po.html):

    PipeOpTaskSurvClassifIPCW$new()
    mlr_pipeops$get("trafotask_survclassif_IPCW")
    po("trafotask_survclassif_IPCW")

## Input and Output Channels

PipeOpTaskSurvClassifIPCW has one input channel named "input", and two
output channels, one named "output" and the other "data".

Training transforms the "input"
[TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md) to a
[TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html),
which is the "output". The target column is named `"status"` and
indicates whether **an event occurred** **before the cutoff time**
\\\tau\\ (`1` = yes, `0` = no). The observed times column is removed
from the "output" task. The transformed task has the property
`"weights_learner"` (the \\\omega_i\\). The "data" is `NULL`.

During prediction, the "input"
[TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md) is
transformed to the "output"
[TaskClassif](https://mlr3.mlr-org.com/reference/TaskClassif.html) with
`"status"` as target (again indicating if the event occurred before the
cutoff time). The "data" is a
[data.table](https://rdrr.io/pkg/data.table/man/data.table.html)
containing the observed `times` \\T_i\\ and censoring
indicators/`status` \\\delta_i\\ of each subject as well as the
corresponding `row_ids`. This "data" is only meant to be used with the
[PipeOpPredClassifSurvIPCW](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_classifsurv_IPCW.md).

## Parameters

The parameters are

- `tau` :: [`numeric()`](https://rdrr.io/r/base/numeric.html)  
  Predefined time point for IPCW. Observations with time larger than
  \\\tau\\ are censored. Must be less or equal to the maximum event
  time.

- `eps` :: [`numeric()`](https://rdrr.io/r/base/numeric.html)  
  Small value to replace \\G(t) = 0\\ censoring probabilities to prevent
  infinite weights (a warning is triggered if this happens).

## References

Vock, M D, Wolfson, Julian, Bandyopadhyay, Sunayan, Adomavicius,
Gediminas, Johnson, E P, Vazquez-Benitez, Gabriela, O'Connor, J P
(2016). “Adapting machine learning techniques to censored time-to-event
health record data: A general-purpose approach using inverse probability
of censoring weighting.” *Journal of Biomedical Informatics*, **61**,
119–131.
[doi:10.1016/j.jbi.2016.03.009](https://doi.org/10.1016/j.jbi.2016.03.009)
, <https://www.sciencedirect.com/science/article/pii/S1532046416000496>.

## See also

[pipeline_survtoclassif_IPCW](https://mlr3proba.mlr-org.com/reference/mlr_graphs_survtoclassif_IPCW.md)

Other Transformation PipeOps:
[`mlr_pipeops_trafopred_classifsurv_IPCW`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_classifsurv_IPCW.md),
[`mlr_pipeops_trafopred_classifsurv_disctime`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_classifsurv_disctime.md),
[`mlr_pipeops_trafopred_regrsurv_pem`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafopred_regrsurv_pem.md),
[`mlr_pipeops_trafotask_survclassif_disctime`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafotask_survclassif_disctime.md),
[`mlr_pipeops_trafotask_survregr_pem`](https://mlr3proba.mlr-org.com/reference/mlr_pipeops_trafotask_survregr_pem.md)

## Super class

[`mlr3pipelines::PipeOp`](https://mlr3pipelines.mlr-org.com/reference/PipeOp.html)
-\> `PipeOpTaskSurvClassifIPCW`

## Methods

### Public methods

- [`PipeOpTaskSurvClassifIPCW$new()`](#method-PipeOpTaskSurvClassifIPCW-new)

- [`PipeOpTaskSurvClassifIPCW$clone()`](#method-PipeOpTaskSurvClassifIPCW-clone)

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

    PipeOpTaskSurvClassifIPCW$new(id = "trafotask_survclassif_IPCW")

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier of the resulting object.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    PipeOpTaskSurvClassifIPCW$clone(deep = FALSE)

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

  # split task to train and test subtasks
  part = partition(task)
  task_train = task$clone()$filter(part$train)
  task_test = task$clone()$filter(part$test)

  # define IPCW pipeop
  po_ipcw = po("trafotask_survclassif_IPCW", tau = 365)

  # during training, output is a classification task with weights
  task_classif_train = po_ipcw$train(list(task_train))[[1]]
  task_classif_train

  # during prediction, output is a classification task (no weights)
  task_classif_test = po_ipcw$predict(list(task_test))[[1]]
  task_classif_test

  # train classif learner on the train task with weights
  learner = lrn("classif.rpart", predict_type = "prob")
  learner$train(task_classif_train)

  # predict using the test output task
  p = learner$predict(task_classif_test)

  # use classif measures for evaluation
  p$confusion
  p$score()
  p$score(msr("classif.auc"))
} # }
```
