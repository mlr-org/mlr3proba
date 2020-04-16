#' @title Rats Survival Task
#' @name mlr_tasks_rats
#' @template task
#' @templateVar type Surv
#' @templateVar ftype survival
#' @templateVar id rats
#' @templateVar data rats
#' @details
#' Column "sex" has been converted to a factor, all others have been converted to integer.
NULL
load_rats = function() {
  data = load_dataset("rats", "survival")
  data = map_at(data, c("rx", "time", "status"), as.integer)
  data$sex = factor(data$sex, levels = c("f", "m"))

  b = as_data_backend(data)
  b$hash = "_mlr3_survival_rats_"
  TaskSurv$new("rats", b, time = "time", event = "status")
}
