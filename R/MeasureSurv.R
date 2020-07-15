#' @title Survival Measure
#'
#' @description
#' This measure specializes [Measure][mlr3::Measure] for survival problems.
#'
#' * `task_type` is set to `"surv"`.
#' * Possible values for `predict_type` are `"distr"`, `"lp"`, `"crank"`, and `"response"`.
#'
#' Predefined measures can be found in the [dictionary][mlr3misc::Dictionary] [mlr3::mlr_measures].
#'
#' @template param_id
#' @template param_range
#' @template param_minimize
#' @template param_average
#' @template param_aggregator
#' @template param_predict_type
#' @template param_measure_properties
#' @template param_predict_sets
#' @template param_task_properties
#' @template param_packages
#' @template param_man
#' @template param_se
#'
#' @family Measure
#' @seealso
#' Default survival measures: [`surv.harrellC`][mlr_measures_surv.harrellC]
#' @export
MeasureSurv = R6Class("MeasureSurv",
  inherit = Measure, cloneable = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, range, minimize = NA, aggregator = NULL, properties = character(),
                          predict_type = "distr", task_properties = character(),
                          packages = character(), man = NA_character_, se = FALSE) {
      super$initialize(id,
        task_type = "surv", range = range, minimize = minimize, aggregator = aggregator,
        properties = properties, predict_type = predict_type, task_properties = task_properties,
        packages = packages, man = man)
    },

    #' @description
    #' Printer.
    print = function() {
      super$print()
      catf(str_indent("* Return type:", ifelse(is.null(private$.se), "Score",
                                               ifelse(private$.se, "Standard Error", "Score"))))
    }
  )
)
