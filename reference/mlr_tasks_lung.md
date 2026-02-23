# Lung Cancer Survival Task

A survival task for the
[lung](https://rdrr.io/pkg/survival/man/lung.html) data set.

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

    mlr_tasks$get("lung")
    tsk("lung")

## Meta Information

- Task type: “surv”

- Dimensions: 168x9

- Properties: -

- Has Missings: `FALSE`

- Target: “time”, “status”

- Features: “age”, “meal.cal”, “pat.karno”, “ph.ecog”, “ph.karno”,
  “sex”, “wt.loss”

## Pre-processing

- Column `inst` has been removed.

- Column `sex` has been converted to a `factor`, all others have been
  converted to `integer`.

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
[`mlr_tasks_mgus`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_mgus.md),
[`mlr_tasks_precip`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_precip.md),
[`mlr_tasks_rats`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_rats.md),
[`mlr_tasks_veteran`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_veteran.md),
[`mlr_tasks_whas`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_whas.md)
