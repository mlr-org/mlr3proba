#' @title Survival Task
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Task]/[TaskSupervised].
#'
#' @description
#' This task specializes [mlr3::Task] and [mlr3::TaskSupervised] for right-censored survival problems.
#' The target column is assumed to be a factor.
#' Predefined tasks are stored in [mlr3::mlr_tasks].
#'
#' The `task_type` is set to `"surv"`.
#'
#' @section Construction:
#' ```
#' t = TaskSurv$new(id, backend, time, time2, event, type = "right")
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Name of the task.
#'
#' * `backend` :: [DataBackend]
#'
#' * `time` :: `numeric()`\cr
#'   Event time if data is right censored. Starting time if interval censored.
#'
#' * `event` :: `integer()` | `logical()`\cr
#'   Event indicator.
#'   If data is right censored then "0"/`FALSE` means alive (no event),
#'   "1"/`TRUE` means dead (event). If data is interval censored then "0" means right censored,
#'   "1" means dead (event), "2" means left censored, "3" means interval censored.
#'
#' * `time2` :: `numeric()`\cr
#'   Ending time for interval censored data. Ignored otherwise.
#'
#' * `type` :: character()\cr
#'    Type of censoring. One of: "right", "left", "counting", "interval", "interval2" or "mstate".
#'
#' @section Fields:
#' All fields from [mlr3::TaskSupervised], and additionally:
#'
#' * `censtype :: character()`\cr
#'    Returns the type of censoring, one of "right", "left", "counting", "interval", "interval2" or "mstate".
#'
#'
#' @section Methods:
#' See [mlr3::TaskSupervised].
#'
#' @family Task
#' @export
#' @examples
#' library(mlr3)
#' lung = mlr3misc::load_dataset("lung", package = "survival")
#' lung$status = (lung$status == 2L)
#' b = as_data_backend(lung)
#' task = TaskSurv$new("lung", backend = b, time = "time", event = "status")
#'
#' task$target_names
#' task$feature_names
#' task$formula()
#' task$truth()
TaskSurv = R6::R6Class("TaskSurv",
                       inherit = TaskSupervised,
                       public = list(
                         initialize = function(id, backend, time, event, time2, type = "right") {
                           if(type %in% c("right", "left", "mstate"))
                              super$initialize(id = id, task_type = "surv", backend = backend,
                                               target = c(time, event))
                           else
                             super$initialize(id = id, task_type = "surv", backend = backend,
                                              target = c(time, time2, event))

                           event = self$data(cols = event)[[1L]]
                           if (!is.logical(event)) {
                             assert_integerish(event, lower = 0, upper = 1)
                           }

                           assert_choice(type, c("right","left","counting","interval","interval2","mstate"))
                           private$.censtype = type

                         },

                         truth = function(rows = NULL) {
                           # truth is defined as the survival outcome as a Survival object
                           tn = self$target_names
                           d = self$data(rows, cols = self$target_names)
                           if(length(tn) == 2)
                             return(Surv(d[[tn[1L]]], as.logical(d[[tn[2L]]]), type = self$censtype))
                           else
                             return(Surv(d[[tn[1L]]], d[[tn[2L]]], as.logical(d[[tn[3L]]]), type = self$censtype))

                         },

                         formula = function(rhs = NULL) {
                           # formula appends the rhs argument to Surv(time, event)~
                           tn = self$target_names
                           if(length(tn) == 2)
                            lhs = sprintf("Surv(%s, %s, type = '%s')", tn[1L], tn[2L], self$censtype)
                           else
                             lhs = sprintf("Surv(%s, %s, %s, type = '%s')", tn[1L], tn[2L], tn[3L], self$censtype)
                           formulate(lhs, rhs %??% ".", env = getNamespace("survival"))
                         }
                       ),

                       active = list(
                         censtype = function(){
                           return(private$.censtype)
                         }
                       ),

                       private = list(
                         .censtype = character()
                       )
)
