# Density Task

This task specializes
[TaskUnsupervised](https://mlr3.mlr-org.com/reference/TaskUnsupervised.html)
for density estimation problems. The data in `backend` should be a
numeric vector or a one column matrix-like object. The `task_type` is
set to `"density"`.

Predefined tasks are stored in the
[dictionary](https://mlr3misc.mlr-org.com/reference/Dictionary.html)
[mlr_tasks](https://mlr3.mlr-org.com/reference/mlr_tasks.html).

## See also

Other Task:
[`TaskSurv`](https://mlr3proba.mlr-org.com/reference/TaskSurv.md),
[`mlr_tasks_actg`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_actg.md),
[`mlr_tasks_faithful`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_faithful.md),
[`mlr_tasks_gbcs`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_gbcs.md),
[`mlr_tasks_gbsg`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_gbsg.md),
[`mlr_tasks_grace`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_grace.md),
[`mlr_tasks_lung`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_lung.md),
[`mlr_tasks_mgus`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_mgus.md),
[`mlr_tasks_precip`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_precip.md),
[`mlr_tasks_rats`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_rats.md),
[`mlr_tasks_veteran`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_veteran.md),
[`mlr_tasks_whas`](https://mlr3proba.mlr-org.com/reference/mlr_tasks_whas.md)

## Super classes

[`mlr3::Task`](https://mlr3.mlr-org.com/reference/Task.html) -\>
[`mlr3::TaskUnsupervised`](https://mlr3.mlr-org.com/reference/TaskUnsupervised.html)
-\> `TaskDens`

## Methods

### Public methods

- [`TaskDens$new()`](#method-TaskDens-new)

- [`TaskDens$clone()`](#method-TaskDens-clone)

Inherited methods

- [`mlr3::Task$add_strata()`](https://mlr3.mlr-org.com/reference/Task.html#method-add_strata)
- [`mlr3::Task$cbind()`](https://mlr3.mlr-org.com/reference/Task.html#method-cbind)
- [`mlr3::Task$data()`](https://mlr3.mlr-org.com/reference/Task.html#method-data)
- [`mlr3::Task$divide()`](https://mlr3.mlr-org.com/reference/Task.html#method-divide)
- [`mlr3::Task$droplevels()`](https://mlr3.mlr-org.com/reference/Task.html#method-droplevels)
- [`mlr3::Task$filter()`](https://mlr3.mlr-org.com/reference/Task.html#method-filter)
- [`mlr3::Task$format()`](https://mlr3.mlr-org.com/reference/Task.html#method-format)
- [`mlr3::Task$formula()`](https://mlr3.mlr-org.com/reference/Task.html#method-formula)
- [`mlr3::Task$head()`](https://mlr3.mlr-org.com/reference/Task.html#method-head)
- [`mlr3::Task$help()`](https://mlr3.mlr-org.com/reference/Task.html#method-help)
- [`mlr3::Task$levels()`](https://mlr3.mlr-org.com/reference/Task.html#method-levels)
- [`mlr3::Task$materialize_view()`](https://mlr3.mlr-org.com/reference/Task.html#method-materialize_view)
- [`mlr3::Task$missings()`](https://mlr3.mlr-org.com/reference/Task.html#method-missings)
- [`mlr3::Task$print()`](https://mlr3.mlr-org.com/reference/Task.html#method-print)
- [`mlr3::Task$rbind()`](https://mlr3.mlr-org.com/reference/Task.html#method-rbind)
- [`mlr3::Task$rename()`](https://mlr3.mlr-org.com/reference/Task.html#method-rename)
- [`mlr3::Task$select()`](https://mlr3.mlr-org.com/reference/Task.html#method-select)
- [`mlr3::Task$set_col_roles()`](https://mlr3.mlr-org.com/reference/Task.html#method-set_col_roles)
- [`mlr3::Task$set_levels()`](https://mlr3.mlr-org.com/reference/Task.html#method-set_levels)
- [`mlr3::Task$set_row_roles()`](https://mlr3.mlr-org.com/reference/Task.html#method-set_row_roles)

------------------------------------------------------------------------

### Method `new()`

Creates a new instance of this
[R6](https://rdrr.io/pkg/R6/man/R6Class.html) class.

#### Usage

    TaskDens$new(id, backend, label = NA_character_)

#### Arguments

- `id`:

  (`character(1)`)  
  Identifier for the new instance.

- `backend`:

  ([mlr3::DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html))  
  Either a
  [DataBackend](https://mlr3.mlr-org.com/reference/DataBackend.html), a
  matrix-like object, or a numeric vector. If weights are used then two
  columns expected, otherwise one column. The weight column must be
  clearly specified (via `[Task]$col_roles`) or the learners will break.

- `label`:

  (`character(1)`)  
  Label for the new instance.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    TaskDens$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
task = TaskDens$new("precip", backend = precip)
task$task_type
#> [1] "dens"
```
