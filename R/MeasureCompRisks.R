#' @title Competing Risks Measure
#'
#' @description
#' This measure specializes [Measure][mlr3::Measure] for competing risk problems.
#'
#' * `task_type` is set to `"cmprsk"`.
#' * `predict_type` is set to `"cif"`.
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
#' @param obs_loss (`function` or `NULL`)\cr
#'   The observation-wise loss function, e.g. [zero-one][mlr3measures::zero_one] for classification error.
#' @param trafo (`list()` or `NULL`)\cr
#'   An optional list with two elements, containing the transformation `"fn"` and its derivative `"deriv"`.
#'   The transformation function is the function that is applied after aggregating the pointwise losses, i.e.
#'   this requires an `$obs_loss` to be present. An example is `sqrt` for RMSE (regression).
#'
#' @family Measure
#' @seealso
#' Default survival measure: `TODO`
#' @export
MeasureCompRisks = R6Class(
  "MeasureCompRisks",
  inherit = Measure,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, param_set = ps(), range, minimize = NA, average = "macro",
                          aggregator = NULL, obs_loss = NULL, properties = character(), predict_type = "cif",
                          predict_sets = "test", task_properties = character(), packages = character(),
                          label = NA_character_, man = NA_character_, trafo = NULL) {

      super$initialize(
        id,
        task_type = "cmprsk",
        param_set = param_set,
        range = range,
        minimize = minimize,
        average = average,
        aggregator = aggregator,
        obs_loss = obs_loss,
        properties = properties,
        predict_type = predict_type,
        predict_sets = predict_sets,
        task_properties = task_properties,
        packages = c("mlr3proba", packages),
        label = label,
        man = man,
        trafo = NULL
      )
    }
  )
)
