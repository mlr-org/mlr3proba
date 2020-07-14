#' @title Survival Task
#'
#' @description
#' This task specializes [mlr3::Task] and [mlr3::TaskSupervised] for possibly-censored survival
#' problems. The target is comprised of survival times and an event indicator.
#' Predefined tasks are stored in [mlr3::mlr_tasks].
#'
#' The `task_type` is set to `"surv"`.
#'
#' @template param_rows
#' @template param_id
#' @template param_backend
#'
#' @family Task
#' @export
#' @examples
#' library(mlr3)
#' lung = mlr3misc::load_dataset("lung", package = "survival")
#' lung$status = (lung$status == 2L)
#' b = as_data_backend(lung)
#' task = TaskSurv$new("lung",
#'   backend = b, time = "time",
#'   event = "status")
#'
#' task$target_names
#' task$feature_names
#' task$formula()
#' task$truth()
TaskSurv = R6::R6Class("TaskSurv",
  inherit = TaskSupervised,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param time (`character(1)`)\cr
    #' Name of the column for event time if data is right censored, otherwise starting time if
    #' interval censored.
    #'
    #' @param event (`character(1)`)\cr
    #' Name of the column giving the event indicator.
    #' If data is right censored then "0"/`FALSE` means alive (no event), "1"/`TRUE` means dead
    #' (event). If `type` is `"interval"` then "0" means right censored, "1" means dead (event),
    #' "2" means left censored, and "3" means interval censored. If `type` is `"interval2"` then
    #' `event` is ignored.
    #'
    #' @param time2 (`character(1)`)\cr
    #' Name of the column for ending time for interval censored data, otherwise ignored.
    #'
    #' @param type (`character(1)`)\cr
    #' Name of the column giving the type of censoring. Default is 'right' censoring.
    initialize = function(id, backend, time, event, time2,
      type = c("right", "left", "counting", "interval", "interval2", "mstate")) {
      type = match.arg(type)

      if (type %in% c("right", "left", "mstate")) {
        super$initialize(
          id = id, task_type = "surv", backend = backend,
          target = c(time, event))
      } else if (type %in% "interval2") {
        super$initialize(
          id = id, task_type = "surv", backend = backend,
          target = c(time, time2))
      } else {
        super$initialize(
          id = id, task_type = "surv", backend = backend,
          target = c(time, time2, event))
      }

      if (type %nin% "interval2") {
        event = self$data(cols = event)[[1L]]
        if (!is.logical(event)) {
          assert_integerish(event, lower = 0, upper = 3)
        }
      }

      private$.censtype = type
    },

    #' @description
    #' True response for specified `row_ids`. Format depends on the task type.
    #' Defaults to all rows with role "use".
    #' @return `numeric()`.
    truth = function(rows = NULL) {
      # truth is defined as the survival outcome as a Survival object
      tn = self$target_names
      d = self$data(rows, cols = self$target_names)
      args = list(time = d[[tn[1L]]], type = self$censtype)
      if (self$censtype %in% "interval2") {
        args$time2 = d[[tn[2L]]]
      } else if (length(tn) == 2) {
        args$event = as.integer(d[[tn[2L]]])
      } else {
        args$event = as.integer(d[[tn[3L]]])
        args$time2 = d[[tn[2L]]]
      }

      if (allMissing(args$event) & allMissing(args$time)) {
        return(suppressWarnings(invoke(Surv, .args = args)))
      } else {
        return(invoke(Surv, .args = args))
      }
    },

    #' @description
    #' Creates a formula for survival models with [survival::Surv] on the LHS.
    #'
    #' @param rhs
    #' If `NULL` RHS is `.`, otherwise gives RHS of formula.
    #'
    #' @return `numeric()`.
    formula = function(rhs = NULL) {
      # formula appends the rhs argument to Surv(time, event)~
      tn = self$target_names
      if (self$censtype %in% "interval2") {
        lhs = sprintf("Surv(time = %s, time2 = %s, type = 'interval2')", tn[1L], tn[2L])
      } else if (length(tn) == 2) {
        lhs = sprintf("Surv(%s, %s, type = '%s')", tn[1L], tn[2L], self$censtype)
      } else {
        lhs = sprintf("Surv(%s, %s, %s, type = '%s')", tn[1L], tn[2L], tn[3L], self$censtype)
      }
      formulate(lhs, rhs %??% ".", env = getNamespace("survival"))
    }
  ),

  active = list(
    #' @field censtype `character(1)`\cr
    #' Returns the type of censoring, one of "right", "left", "counting", "interval", "interval2"
    #' or "mstate".
    censtype = function() {
      return(private$.censtype)
    }
  ),

  private = list(
    .censtype = character()
  )
)
