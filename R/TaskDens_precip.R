#' @title Annual Precipitation Density Task
#' @name mlr_tasks_precip
#' @template task
#' @templateVar type Dens
#' @templateVar ftype density
#' @templateVar id precip
#' @templateVar data precip
NULL
load_precip = function(id = "precip") {
  b = as_data_backend(data.table::data.table(precip = load_dataset("precip", "datasets", keep_rownames = TRUE)))
  b$hash = "_mlr3_tasks_precip_"
  TaskDens$new(id, b, target = "precip")
}
