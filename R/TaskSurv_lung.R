#' @title Lung Cancer Survival Task
#'
#' @name mlr_tasks_lung
#' @format [R6::R6Class] inheriting from [TaskSurv].
#'
#' @section Construction:
#' ```
#' mlr3::mlr_tasks$get("lung")
#' mlr3::tsk("lung")
#' ```
#'
#' @description
#' A survival task for the [survival::lung] data set.
#' Columns have been converted to integer, column "sex" has been converted to a factor.
#' @template seealso_task
NULL

load_lung = function() {
  data = load_dataset("lung", "survival")
  data = map_dtc(data, as.integer)
  data$status = (data$status == 2L)
  data$sex = factor(ifelse(data$sex == 1L, "m", "f"), levels = c("f", "m"))

  b = as_data_backend(data)
  b$hash = "_mlr3_survival_lung_"
  TaskSurv$new("lung", b, time = "time", status = "status")
}
