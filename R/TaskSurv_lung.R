#' @title Lung Cancer Survival Task
#' @name mlr_tasks_lung
#' @template task
#' @templateVar type Surv
#' @templateVar ftype survival
#' @templateVar id lung
#' @templateVar data lung
#' @details
#' Column "sex" has been converted to a factor, all others have been converted to integer.
NULL
load_lung = function() {
  data = load_dataset("lung", "survival")
  data = map_dtc(data, as.integer)
  data$status = (data$status == 2L)
  data$sex = factor(ifelse(data$sex == 1L, "m", "f"), levels = c("f", "m"))

  b = as_data_backend(data)
  b$hash = "_mlr3_survival_lung_"
  TaskSurv$new("lung", b, time = "time", event = "status")
}
