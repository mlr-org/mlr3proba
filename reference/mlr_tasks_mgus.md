# Monoclonal Gammopathy Survival Task

A survival task for the
[mgus](https://rdrr.io/pkg/survival/man/mgus.html) data set.

## Format

[R6::R6Class](https://rdrr.io/pkg/R6/man/R6Class.html) inheriting from
[TaskSurv](https://mlr3proba.mlr-org.com/reference/TaskSurv.md).

## Dictionary

This [Task](https://mlr3.mlr-org.com/reference/Task.html) can be
instantiated via the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_tasks](https://mlr3.mlr-org.com/reference/mlr_tasks.html) or with
the associated sugar function
[tsk()](https://mlr3.mlr-org.com/reference/mlr_sugar.html):

    mlr_tasks$get("mgus")
    tsk("mgus")

## Meta Information

- Task type: “surv”

- Dimensions: 176x9

- Properties: -

- Has Missings: `FALSE`

- Target: “time”, “status”

- Features: “age”, “alb”, “creat”, “dxyr”, “hgb”, “mspike”, “sex”

## Pre-processing

- Removed columns `id`, `pcdx` and `pctime`.

- Renamed target columns from (`fultime`, `death`) to (`time`,
  `status`).

- Kept only complete cases (no missing values).

## See also

- Chapter in the [mlr3book](https://mlr3book.mlr-org.com/):
  <https://mlr3book.mlr-org.com/chapters/chapter2/data_and_basic_modeling.html>

- [Dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
  of [Tasks](https://mlr3.mlr-org.com/reference/Task.html):
  [mlr3::mlr_tasks](https://mlr3.mlr-org.com/reference/mlr_tasks.html)

- `as.data.table(mlr_tasks)` for a table of available
  [Tasks](https://mlr3.mlr-org.com/reference/Task.html) in the running
  session

Other Task:
[`TaskDens`](https://mlr3proba.mlr-org.com/reference/TaskDens.md),
[`TaskSurv`](https://mlr3proba.mlr-org.com/reference/TaskSurv.md),
[`mlr_tasks_actg`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_actg.md),
[`mlr_tasks_faithful`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_faithful.md),
[`mlr_tasks_gbcs`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_gbcs.md),
[`mlr_tasks_gbsg`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_gbsg.md),
[`mlr_tasks_grace`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_grace.md),
[`mlr_tasks_lung`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_lung.md),
[`mlr_tasks_precip`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_precip.md),
[`mlr_tasks_rats`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_rats.md),
[`mlr_tasks_veteran`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_veteran.md),
[`mlr_tasks_whas`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_whas.md)
