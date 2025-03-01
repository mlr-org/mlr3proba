#' @title Competing Risks Task
#'
#' @description
#' This task extends [mlr3::Task] and [mlr3::TaskSupervised] to handle survival
#' problems with **competing risks**.
#' The target variable consists of survival times and an event indicator, which
#' must be a non-negative integer in the set \eqn{(0,1,2,...,K)}.
#' \eqn{0} represents censored observations, while other integers correspond to
#' distinct competing events.
#' Every row corresponds to one subject/observation.
#'
#' Predefined tasks are stored in [mlr3::mlr_tasks].
#'
#' The `task_type` is set to `"cmprsk"`.
#'
#' **Note:** Currently only right-censoring is supported.
#'
#' @template param_rows
#'
#' @family Task
#' @examples
#' library(mlr3)
#' task = tsk("pbc")
#'
#' # meta data
#' task$target_names # target is always (time, status) for right-censoring tasks
#' task$feature_names
#' task$formula()
#'
#' # survival data
#' task$truth() # survival::Surv() object
#' task$times() # (unsorted) times
#' task$event() # event indicators (0 = censored, >0 = different causes)
#' task$unique_times() # sorted unique times
#' task$unique_event_times() # sorted unique event times (from any cause)
#' task$aalen_johansen(strata = "sex") # Aalen-Johansen estimator
#'
#' # proportion of censored observations across all dataset
#' task$cens_prop()
#'
#' @export
TaskCompRisks = R6Class("TaskCompRisks",
  inherit = TaskSupervised,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @details
    #' Only right-censoring competing risk tasks are currently supported.
    #'
    #' @template param_id
    #' @template param_backend
    #' @param time (`character(1)`)\cr
    #'  Name of the column for event time.
    #' @param event (`character(1)`)\cr
    #'  Name of the column giving the event indicator (\eqn{0} corresponds to
    #'  censoring, values \eqn{> 0} correspond to competing events.
    #' @param label (`character(1)`)\cr
    #'  Label for the new instance.
    initialize = function(id, backend, time = "time", event = "event",
                          label = NA_character_) {
      # only right-censoring supported
      private$.cens_type = "right"
      backend = as_data_backend(backend)

      # check event is an integer starting from 0
      event_col = get_private(backend)$.data[, event, with = FALSE][[1L]]
      assert_integerish(event_col, lower = 0L, any.missing = FALSE)

      # check that there is at least two competing events
      n_cmp_events = sum(unique(event_col) != 0)
      if (n_cmp_events < 2) {
        stopf("Define at least two competing events, there are only %i in the data",
              n_cmp_events)
      }

      # keep all the event levels
      private$.event_levels = levels(as.factor(event_col))

      super$initialize(
        id = id, task_type = "cmprsk", backend = backend,
        target = c(time, event), label = label
      )
    },

    #' @description
    #' True response for specified `row_ids`. This is the multi-state format
    #' using [Surv][survival::Surv()] with the `event` target column as a `factor`:
    #' `Surv(time, as.factor(event))`
    #'
    #' Defaults to all rows with role `"use"`.
    #'
    #' @return [survival::Surv()].
    truth = function(rows = NULL) {
      tn = self$target_names
      data = self$data(rows = rows, cols = self$target_names)
      times = data[[tn[1L]]]
      events = data[[tn[2L]]]

      args = list(
        time = times,
        # levels is needed, otherwise subsetting `Surv()` doesn't work as it should
        event = factor(events, levels = c("0", self$cmp_events))
      )

      invoke(Surv, .args = args)
    },

    #' @description
    #' Creates a formula for competing risk models with [survival::Surv()] on
    #' the LHS (left hand side).
    #'
    #' @param rhs
    #' If `NULL`, RHS (right hand side) is `"."`, otherwise RHS is `"rhs"`.
    #'
    #' @return [stats::formula()].
    formula = function(rhs = NULL) {
      tn = self$target_names
      lhs = sprintf("Surv(%s, as.factor(%s))", tn[1L], tn[2L])
      formulate(lhs, rhs %??% ".", env = getNamespace("survival"))
    },

    #' @description
    #' Returns the (unsorted) outcome times.
    #' @return `numeric()`
    times = function(rows = NULL) {
      truth = self$truth(rows)
      as.numeric(truth[, 1L])
    },

    #' @description
    #' Returns the event indicator.
    #' @return `integer()`
    event = function(rows = NULL) {
      truth = self$truth(rows)
      as.integer(truth[, 2L])
    },

    #' @description
    #' Returns the unique events (excluding censoring).
    #' @return `integer()`
    unique_events = function(rows = NULL) {
      events = self$event(rows)
      sort(setdiff(events, 0))
    },

    #' @description
    #' Returns the sorted unique outcome times.
    #' @return `numeric()`
    unique_times = function(rows = NULL) {
      sort(unique(self$times(rows)))
    },

    #' @description
    #' Returns the sorted unique event outcome times (by any cause).
    #' @return `numeric()`
    unique_event_times = function(rows = NULL) {
      sort(unique(self$times(rows)[self$event(rows) != 0]))
    },

    #' @description
    #' Calls [survival::survfit()] to calculate the Aalen–Johansen estimator.
    #'
    #' @param strata (`character()`)\cr
    #'   Stratification variables to use.
    #' @param rows (`integer()`)\cr
    #'   Subset of row indices.
    #' @param ... (any)\cr
    #'   Additional arguments passed down to [survival::survfit.formula()].
    #' @return [survival::survfit.object].
    aalen_johansen = function(strata = NULL, rows = NULL, ...) {
      assert_character(strata, null.ok = TRUE)
      f = self$formula(strata %??% 1)
      cols = c(self$target_names, intersect(self$backend$colnames, strata))
      data = self$data(rows = rows, cols = cols)
      survival::survfit(f, data = data, ...)
    },

    #' @description
    #' Returns the **proportion of censoring** for this competing risks task.
    #' By default, this is returned for all observations, otherwise only the
    #' specified ones (`rows`).
    #'
    #' @return `numeric()`
    cens_prop = function(rows = NULL) {
      event = self$event(rows)
      total_censored = sum(event == 0)
      n_obs = length(event)

      total_censored / n_obs
    },

    #' @description
    #' Subsets the task, keeping only the rows specified via row ids `rows`.
    #' This operation mutates the task in-place.
    #'
    #' @return Returns the object itself, but modified **by reference.**
    filter = function(rows = NULL) {
      # check that we don't remove the competing events from the data
      uevents = self$unique_events(rows)
      if (length(uevents) != length(self$cmp_events)) {
        stopf("Can't filter task %s: %i competing events found, but row filtering results in %i unique competing event(s)", self$id, length(self$cmp_events), length(uevents)) #nolint
      }

      super$filter(rows)
    }
  ),

  active = list(
    #' @field cens_type (`character(1)`)\cr
    #' Returns the type of censoring.
    #'
    #' Currently, only the `"right"` censoring type is fully supported.
    #' The API might change in the future to support left and interval censoring.
    cens_type = function(rhs) {
      assert_ro_binding(rhs)
      private$.cens_type
    },

    #' @field cmp_events (`character(1)`)\cr
    #' Returns the names of the competing events.
    cmp_events = function(rhs) {
      assert_ro_binding(rhs)
      setdiff(private$.event_levels, "0")
    }
  ),

  private = list(
    .cens_type = NULL,
    .event_levels = NULL
  )
)
