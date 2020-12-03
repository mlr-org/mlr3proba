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
      type = c("right", "left", "counting", "interval", "mstate")) {

      type = match.arg(type)

      event = backend[, "event"][[1L]]
      if (type == "mstate") {
        assert_factor(event)
      } else if (!is.logical(event)) {
        assert_integerish(event, lower = 0, upper = 2)
      }

      private$.censtype = type

      if (type %in% c("right", "left", "mstate")) {
        super$initialize(
          id = id, task_type = "surv", backend = backend,
          target = c(time, event))
      } else {
        super$initialize(
          id = id, task_type = "surv", backend = backend,
          target = c(time, time2, event))
      }
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
      if (length(tn) == 2) {
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
      if (length(tn) == 2) {
        lhs = sprintf("Surv(%s, %s, type = '%s')", tn[1L], tn[2L], self$censtype)
      } else {
        lhs = sprintf("Surv(%s, %s, %s, type = '%s')", tn[1L], tn[2L], tn[3L], self$censtype)
      }
      formulate(lhs, rhs %??% ".", env = getNamespace("survival"))
    }
  ),

  active = list(
    #' @field censtype `character(1)`\cr
    #' Returns the type of censoring, one of "right", "left", "counting", "interval", or "mstate".
    censtype = function() {
      return(private$.censtype)
    },

    #' @field times `numeric()`\cr
    #' Returns the (unsorted) true outcome times.
    times = function(rows = NULL) {
      truth = self$truth(rows)
      if (censtype %in% c("interval", "counting")) {
        return(truth[, 1:2])
      } else {
        return(truth[, 1])
      }
    },

    #' @field status (`integer()`) \cr
    #' Returns the true event indicator.
    status = function(rows = NULL) {
      truth = self$truth(rows)
      if (censtype %in% c("interval", "counting")) {
        status = truth[, 3L]
      } else {
        status = truth[, 2L]
      }

      as.integer(status)
    },

    #' @field unique_times (`numeric()`)\cr
    #' Returns the sorted unique outcome times.
    unique_times = function(rows = NULL) {
      sort(unique(self$times(rows)))
    },

    #' @field unique_event_times (`numeric()`)\cr
    #' Returns the sorted unique event (or failure) times.
    unique_event_times = function(rows = NULL) {
      sort(unique(self$times(rows)[self$status(rows) != 0]))
    },

    #' @field risk_set (`integer()`) \cr
    #' Returns the `row_ids` of the observations 'at risk' (not dead or censored) just before `time`.
    risk_set = function(time = NULL) {
      if (is.null(time)) {
        self$row_ids
      } else {
        self$row_ids[self$times() >= time]
      }
    }
  ),

  private = list(
    .censtype = character()
  )
)
