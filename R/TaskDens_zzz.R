#' @title Annual Precipitation Density Task
#'
#' @name mlr_tasks_precip
#'
#' @templateVar type Dens
#' @templateVar task_type density
#' @templateVar id precip
#' @templateVar data precip
#' @template task
#' @template seealso_task
#'
#' @section Preprocessing:
#' - Only the `precip` column is kept in this task.
#'
NULL

load_precip = function(id = "precip") {
  b = as_data_backend(data.table::data.table(precip = load_dataset("precip", "datasets",
    keep_rownames = TRUE)))
  task = TaskDens$new(id, b, label = "Annual Precipitation")
  b$hash = task$man = "mlr3proba::mlr_tasks_precip"
  task
}

#' @title Old Faithful Eruptions Density Task
#'
#' @name mlr_tasks_faithful
#'
#' @templateVar type Dens
#' @templateVar task_type density
#' @templateVar id faithful
#' @templateVar data faithful
#' @template task
#' @template seealso_task
#'
#' @section Preprocessing:
#' - Only the `eruptions` column is kept in this task.
#'
NULL

load_faithful = function(id = "faithful") {
  b = as_data_backend(data.table::data.table(eruptions = load_dataset("faithful", "datasets",
    keep_rownames = TRUE)$eruptions))
  task = TaskDens$new(id, b, label = "Old Faithful Eruptions")
  b$hash = task$man = "mlr3proba::mlr_tasks_faithful"
  task
}

register_task("precip", load_precip)
register_task("faithful", load_faithful)
