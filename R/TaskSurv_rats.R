#' @title Rats Survival Task
#'
#' @name mlr_tasks_rats
#' @format [R6::R6Class] inheriting from [TaskSurv].
#'
#' @section Construction:
#' ```
#' mlr3::mlr_tasks$get("rats")
#' mlr3::tsk("rats")
#' ```
#'
#' @description
#' A survival task for the [survival::rats] data set.
#' Column "sex" has been converted to a factor, all others have been converted to integer.
#' @template seealso_task
NULL

load_rats = function() {
  data = load_dataset("rats", "survival")
  data = map_at(data, c("rx", "time", "status"), as.integer)
  data$status = as.logical(data$status)
  data$sex = factor(data$sex, levels = c("f", "m"))

  b = as_data_backend(data)
  b$hash = "_mlr3_survival_rats_"
  TaskSurv$new("rats", b, time = "time", event = "status")
}
