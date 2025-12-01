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
#' @template param_param_set
#' @template param_range
#' @template param_minimize
#' @template param_average
#' @template param_aggregator
#' @template param_predict_type
#' @template param_measure_properties
#' @template param_predict_sets
#' @template param_task_properties
#' @template param_packages
#' @template param_label
#' @template param_man
#'
#' @family Measure
#' @seealso
#' Default survival measure: [`surv.cindex`][mlr_measures_surv.cindex]
#' @export
MeasureSurv = R6Class("MeasureSurv",
  inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, param_set = ps(), range, minimize = NA, average = "macro",
      aggregator = NULL, properties = character(), predict_type = "distr",
      predict_sets = "test", task_properties = character(), packages = character(),
      label = NA_character_, man = NA_character_ {

      super$initialize(
        id,
        task_type = "surv",
        param_set = param_set,
        range = range,
        minimize = minimize,
        average = average,
        aggregator = aggregator,
        properties = properties,
        predict_type = predict_type,
        predict_sets = predict_sets,
        task_properties = task_properties,
        packages = c("mlr3proba", packages),
        label = label,
        man = man
      )
    }
  )
)
