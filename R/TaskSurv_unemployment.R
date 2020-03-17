#' @title Unemployment Duration Survival Task
#' @name mlr_tasks_unemployment
#' @template task
#' @templateVar type Surv
#' @templateVar ftype survival
#' @templateVar id unemployment
#' @templateVar data Ecdat::UnempDur
#' @details
#' A survival task for the "UnempDur" data set in package \CRANpkg{Ecdat}.
#' Contains the following columns of the original data set:
#' "spell" (time), "censor1" (status), "age", "ui", "logwage", and "tenure".
NULL
load_unemployment = function() {
  path = file.path(system.file("extdata", package = "mlr3proba"), "unemployment.rds")
  b = as_data_backend(readRDS(path))
  b$hash = "_mlr3_survival_unemployment_"
  TaskSurv$new("unemployment", b, time = "spell", event = "censor1")
}
