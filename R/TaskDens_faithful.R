#' @title Old Faithful Eruptions Density Task
#' @name mlr_tasks_faithful
#' @template task
#' @templateVar type Dens
#' @templateVar ftype density
#' @templateVar id faithful
#' @templateVar data faithful
#' @details Only the `eruptions` column is kept in this task.
NULL
load_faithful = function(id = "faithful") {
  b = as_data_backend(data.table::data.table(precip = load_dataset("faithful", "datasets",
                                                                   keep_rownames = TRUE)$eruptions))
  b$hash = "_mlr3_tasks_faithful_"
  TaskDens$new(id, b, target = "precip")
}
