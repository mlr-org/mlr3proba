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

#' @title Unemployment Duration Survival Task
#' @name mlr_tasks_unemployment
#' @description
#' A survival task for the `UnempDur` data set.
#'
#' @format [R6::R6Class] inheriting from [TaskSurv].
#'
#' @section Construction:
#' ```
#' mlr3::mlr_tasks$get("unemployment")
#' mlr3::tsk("unemployment")
#' ```
#' @template seealso_task
#'
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

#' @title ACTG 320 Survival Task
#' @name mlr_tasks_actg
#' @template task
#' @templateVar type Surv
#' @templateVar ftype survival
#' @templateVar id actg
#' @templateVar data actg
#' @details
#' Column "sex" has been renamed to "sexF" and "censor" has been renamed to "status".
#' Columns "id", "time_d", and "censor_d" have been removed so target is time to AIDS diagnosis
#' or death.
NULL
load_actg = function() {
  data = load_dataset("actg", "mlr3proba")
  data[,c("id", "time_d", "censor_d")] = NULL
  colnames(data)[6] = "sexF"
  colnames(data)[2] = "status"
  b = as_data_backend(data)
  b$hash = "_mlr3_survival_actg_"
  TaskSurv$new("actg", b, time = "time", event = "status")
}

#' @title German Breast Cancer Study Survival Task
#' @name mlr_tasks_gbcs
#' @template task
#' @templateVar type Surv
#' @templateVar ftype survival
#' @templateVar id gbcs
#' @templateVar data gbcs
#' @details
#' Column "id" and all date columns removed, as well as "rectime" and "censrec".
#' Target is time to death.
NULL
load_gbcs = function() {
  data = load_dataset("gbcs", "mlr3proba")
  data[,c("id", "diagdate", "recdate", "deathdate", "rectime", "censrec")] = NULL
  colnames(data)[9:10] = c("time", "status")
  b = as_data_backend(data)
  b$hash = "_mlr3_survival_gbcs_"
  TaskSurv$new("gbcs", b, time = "time", event = "status")
}

#' @title GRACE 1000 Survival Task
#' @name mlr_tasks_grace
#' @template task
#' @templateVar type Surv
#' @templateVar ftype survival
#' @templateVar id grace
#' @templateVar data grace
#' @details
#' Column "id" is removed. Columns "days" and "death" renamed to "time" and "status" resp.
NULL
load_grace = function() {
  data = load_dataset("grace", "mlr3proba")
  data[,c("id")] = NULL
  colnames(data)[1:2] = c("time", "status")
  b = as_data_backend(data)
  b$hash = "_mlr3_survival_grace_"
  TaskSurv$new("grace", b, time = "time", event = "status")
}

#' @title Worcester Heart Attack Study (WHAS) Survival Task
#' @name mlr_tasks_whas
#' @template task
#' @templateVar type Surv
#' @templateVar ftype survival
#' @templateVar id whas
#' @templateVar data whas
#' @details
#' Columns "id", "yrgrp", and "dstat" are removed so target is status at last follow-up.
#' Column "sex" renamed to "sexF", "lenfol" to "time", and "fstat" to "status".
NULL
load_whas = function() {
  data = load_dataset("whas", "mlr3proba")
  data[,c("id", "yrgrp", "dstat")] = NULL
  colnames(data)[2] = "sexF"
  colnames(data)[10:11] = c("time", "status")
  b = as_data_backend(data)
  b$hash = "_mlr3_survival_whas_"
  TaskSurv$new("whas", b, time = "time", event = "status")
}
