#' @title Veteran Survival Task
#'
#' @name mlr_tasks_veteran
#' @templateVar type Surv
#' @templateVar task_type survival
#' @templateVar id veteran
#' @templateVar data veteran
#' @template task
#' @template seealso_task
#'
#' @section Pre-processing:
#' - Columns `age`, `time`, `status`, `diagtime` and `karno` have been converted
#' to `integer`.
#' - Columns `trt`, `prior` have been converted to `factor`s. Prior therapy
#' values are 'no' vs 'yes' instead of 0 vs 10.
NULL

load_veteran = function() {
  data = survival::veteran
  data = map_at(data, c("age", "time", "status", "diagtime", "karno"), as.integer)
  data = map_at(data, c("trt", "prior"), as.factor)
  data$trt = factor(data$trt, levels = c("1", "2"))
  data$prior = factor(ifelse(data$prior == 0, "no", "yes"), levels = c("no", "yes"))

  b = as_data_backend(data)
  task = TaskSurv$new("veteran", b, time = "time", event = "status", label = "Veteran")
  b$hash = task$man = "mlr3proba::mlr_tasks_veteran"

  task
}

#' @title Rats Survival Task
#'
#' @name mlr_tasks_rats
#' @templateVar type Surv
#' @templateVar task_type survival
#' @templateVar id rats
#' @templateVar data rats
#' @template task
#' @template seealso_task
#'
#' @section Pre-processing:
#' - Column `sex` has been converted to a `factor`, all others have been
#' converted to `integer`.
#'
NULL

load_rats = function() {
  data = survival::rats
  data = map_at(data, c("rx", "time", "status"), as.integer)
  data$sex = factor(data$sex, levels = c("f", "m"))

  b = as_data_backend(data)
  task = TaskSurv$new("rats", b, time = "time", event = "status", label = "Rats")
  b$hash = task$man = "mlr3proba::mlr_tasks_rats"

  task
}

#' @title Unemployment Duration Survival Task
#'
#' @name mlr_tasks_unemployment
#'
#' @templateVar type Surv
#' @templateVar task_type survival
#' @templateVar id unemployment
#' @templateVar data Ecdat::UnempDur
#' @template task
#' @template seealso_task
#'
#' @section Preprocessing:
#' - Only the columns `spell`, `censor1`, `age`, `logwage`, `tenure`, `ui` are
#' kept in this task.
#'
NULL

load_unemployment = function() {
  path = file.path(system.file("extdata", package = "mlr3proba"), "unemployment.rds")

  b = as_data_backend(readRDS(path))
  task = TaskSurv$new("unemployment", b, time = "spell", event = "censor1", label = "Unemployment Duration")
  b$hash = task$man = "mlr3proba::mlr_tasks_unemployment"

  task
}

#' @title Lung Cancer Survival Task
#'
#' @name mlr_tasks_lung
#'
#' @templateVar type Surv
#' @templateVar task_type survival
#' @templateVar id lung
#' @templateVar data lung
#' @template task
#' @template seealso_task
#'
#' @section Pre-processing:
#' - Column `sex` has been converted to a `factor`, all others have been
#' converted to `integer`.
#'
NULL

load_lung = function() {
  data = survival::lung
  data = map_dtc(data, as.integer)
  data$status = as.integer(data$status == 2L)
  data$sex = factor(ifelse(data$sex == 1L, "m", "f"), levels = c("f", "m"))

  b = as_data_backend(data)
  task = TaskSurv$new("lung", b, time = "time", event = "status", label = "Lung Cancer")
  b$hash = task$man = "mlr3proba::mlr_tasks_lung"

  task
}

#' @title ACTG 320 Survival Task
#'
#' @name mlr_tasks_actg
#'
#' @templateVar type Surv
#' @templateVar task_type survival
#' @templateVar id actg
#' @templateVar data actg
#' @template task
#' @template seealso_task
#'
#' @section Pre-processing:
#' - Column `sex` has been renamed to `sexF` and `censor` has been renamed to `status`.
#' - Columns `id`, `time_d`, and `censor_d` have been removed so target is `time`
#' to AIDS diagnosis (in days).
#'
NULL

load_actg = function() {
  data = load_dataset("actg", "mlr3proba")
  data[, c("id", "time_d", "censor_d")] = NULL
  colnames(data)[6] = "sexF"
  colnames(data)[2] = "status"

  b = as_data_backend(data)
  task = TaskSurv$new("actg", b, time = "time", event = "status", label = "ACTG 320")
  b$hash = task$man = "mlr3proba::mlr_tasks_actg"

  task
}

#' @title German Breast Cancer Study Survival Task
#'
#' @name mlr_tasks_gbcs
#'
#' @templateVar type Surv
#' @templateVar task_type survival
#' @templateVar id gbcs
#' @templateVar data gbcs
#' @template task
#' @template seealso_task
#'
#' @section Preprocessing:
#' - Column `id` and all date columns have been removed, as well as `rectime`
#' and `censrec`.
#' - Column `survtime` has been renamed to `time` and `censdead` to `status`.
#'
NULL

load_gbcs = function() {
  data = load_dataset("gbcs", "mlr3proba")
  data[, c("id", "diagdate", "recdate", "deathdate", "rectime", "censrec")] = NULL
  colnames(data)[9:10] = c("time", "status")

  b = as_data_backend(data)
  task = TaskSurv$new("gbcs", b, time = "time", event = "status", label = "German Breast Cancer")
  b$hash = task$man = "mlr3proba::mlr_tasks_gbcs"

  task
}

#' @title GRACE 1000 Survival Task
#'
#' @name mlr_tasks_grace
#'
#' @templateVar type Surv
#' @templateVar task_type survival
#' @templateVar id grace
#' @templateVar data grace
#' @template task
#' @template seealso_task
#'
#' @section Preprocessing:
#' - Column `id` is removed.
#' - Columns `days` and `death` are renamed to `time` and `status` respectively.
#'
NULL

load_grace = function() {
  data = load_dataset("grace", "mlr3proba")
  data[, c("id")] = NULL
  colnames(data)[1:2] = c("time", "status")

  b = as_data_backend(data)
  task = TaskSurv$new("grace", b, time = "time", event = "status", label = "GRACE 1000")
  b$hash = task$man = "mlr3proba::mlr_tasks_grace"

  task
}

#' @title Worcester Heart Attack Study (WHAS) Survival Task
#'
#' @name mlr_tasks_whas
#'
#' @templateVar type Surv
#' @templateVar task_type survival
#' @templateVar id whas
#' @templateVar data whas
#' @template task
#' @template seealso_task
#'
#' @section Preprocessing:
#' - Columns `id`,  `yrgrp`, and `dstat` are removed
#' - Column `sex` is renamed to `sexF`, `lenfol` to `time`, and `fstat` to `status`.
#' - Target is total follow-up time from hospital admission
#'
NULL

load_whas = function() {
  data = load_dataset("whas", "mlr3proba")
  data[, c("id", "yrgrp", "dstat")] = NULL
  colnames(data)[2] = "sexF"
  colnames(data)[10:11] = c("time", "status")

  b = as_data_backend(data)
  task = TaskSurv$new("whas", b, time = "time", event = "status", label = "Worcester Heart Attack")
  b$hash = task$man = "mlr3proba::mlr_tasks_whas"

  task
}

register_task("veteran", load_veteran)
register_task("rats", load_rats)
register_task("unemployment", load_unemployment)
register_task("lung", load_lung)
register_task("actg", load_actg)
register_task("gbcs", load_gbcs)
register_task("grace", load_grace)
register_task("whas", load_whas)
