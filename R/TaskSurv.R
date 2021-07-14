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
#' lung = survival::lung
#' lung$status = (lung$status == 2L)
#' task = TaskSurv$new("lung",
#'   backend = lung, time = "time",
#'   event = "status")
#'
#' # meta data
#' task$target_names
#' task$feature_names
#' task$formula()
#'
#' # survival data
#' task$truth()
#' task$times()
#' task$status()
#' task$unique_times()
#' task$unique_event_times()
#' task$risk_set(time = 700)
#'
TaskSurv = R6::R6Class("TaskSurv",
  inherit = TaskSupervised,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @template param_time
    #' @template param_event
    #' @template param_time2
    #' @template param_type
    initialize = function(id, backend, time = "time", event = "event", time2,
      type = c("right", "left", "interval", "counting", "interval2", "mstate")) {

      type = match.arg(type)

      backend = as_data_backend(backend)

      if (type != "interval2") {
        c_ev = r6_private(backend)$.data[, event, with = FALSE][[1]]
        if (type == "mstate") {
          assert_factor(c_ev)
        } else if (type == "interval") {
          assert_integerish(c_ev, lower = 0, upper = 3)
        } else if (!is.logical(c_ev)) {
          assert_integerish(c_ev, lower = 0, upper = 2)
        }
      }

      private$.censtype = type

      if (type %in% c("right", "left", "mstate")) {
        super$initialize(
          id = id, task_type = "surv", backend = backend,
          target = c(time, event))
      } else if (type %in% c("interval", "counting")) {
        super$initialize(
          id = id, task_type = "surv", backend = backend,
          target = c(time, time2, event))
      } else {
        super$initialize(
          id = id, task_type = "surv", backend = backend,
          target = c(time, time2))
      }
    },

    #' @description
    #' True response for specified `row_ids`. Format depends on the task type.
    #' Defaults to all rows with role "use".
    #' @return `numeric()`.
    truth = function(rows = NULL) {
      # truth is defined as the survival outcome as a Survival object
      tn = self$target_names
      ct = self$censtype
      d = self$data(rows, cols = self$target_names)
      args = list(time = d[[tn[1L]]], type = self$censtype)

      if (ct %in% c("right", "left", "mstate")) {
        args$event = as.integer(d[[tn[2L]]])
      } else if (ct %in% c("interval", "counting")) {
        args$event = as.integer(d[[tn[3L]]])
        args$time2 = d[[tn[2L]]]
      } else {
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
    },

    #' @description
    #' Returns the (unsorted) outcome times.
    #' @return `numeric()`
    times = function(rows = NULL) {
      truth = self$truth(rows)
      if (self$censtype %in% c("interval", "counting", "interval2")) {
        return(truth[, 1:2])
      } else {
        return(truth[, 1L])
      }
    },

    #' @description
    #' Returns the event indicator (aka censoring/survival indicator).
    #' If `censtype` is `"right"` or `"left"` then `1` is event and `0` is censored.
    #' If `censtype` is `"mstate"` then `0` is censored and all other values are different events.
    #' If `censtype` is `"interval"` then `0` is right-censored, `1` is event, `2` is left-censored,
    #' `3` is interval-censored.
    #' See [survival::Surv].
    #' @return `integer()`
    status = function(rows = NULL) {
      truth = self$truth(rows)
      if (self$censtype %in% c("interval", "counting", "interval2")) {
        status = truth[, 3L]
      } else {
        status = truth[, 2L]
      }

      as.integer(status)
    },

    #' @description
    #' Returns the sorted unique outcome times for 'right', 'left', and 'mcstate'.
    #' @return `numeric()`
    unique_times = function(rows = NULL) {
      if (self$censtype %in% c("interval", "counting", "interval2")) {
        stop("Not implemented for 'interval', 'interval2', or 'counting', 'censtype'.")
      }

      sort(unique(self$times(rows)))
    },

    #' @description
    #' Returns the sorted unique event (or failure) outcome times.
    #' @return `numeric()`
    unique_event_times = function(rows = NULL) {
      if (self$censtype %in% c("interval", "counting", "interval2")) {
        stop("Not implemented for 'interval', 'interval2', or 'counting', 'censtype'.")
      }

      sort(unique(self$times(rows)[self$status(rows) != 0]))
    },

    #' @description
    #' Returns the `row_ids` of the observations 'at risk' (not dead or censored) at `time`.
    #' @param time (`numeric(1)`) \cr Time to return risk set for, if `NULL` returns all `row_ids`.
    #' @return `integer()`
    risk_set = function(time = NULL) {
      if (self$censtype %in% c("interval", "counting", "interval2")) {
        stop("Not implemented for 'interval', 'interval2', or 'counting', 'censtype'.")
      }

      if (is.null(time)) {
        self$row_ids
      } else {
        self$row_ids[self$times() >= time]
      }
    }
  ),

  active = list(
    #' @field censtype `character(1)`\cr
    #' Returns the type of censoring, one of "right", "left", "counting", "interval", or "mstate".
    censtype = function() {
      return(private$.censtype)
    }
  ),

  private = list(
    .censtype = character()
  )
)
