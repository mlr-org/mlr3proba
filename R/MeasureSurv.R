#' @title Survival Measure
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3::Measure].
#'
#' @description
#' This measure specializes [mlr3::Measure] for survival problems.
#' Predefined measures can be found in the [mlr3misc::Dictionary] [mlr3::mlr_measures].
#'
#' @section Construction:
#' ```
#' MeasureSurv$new(id, range, minimize, aggregator = NULL,
#'                 properties = character(), predict_type = "distr",
#'                 task_properties = character(), packages = character())
#' ```
#' For a description of the arguments, see [mlr3::Measure].
#' The `task_type` is set to `"surv"`.
#' Possible values for `predict_type` are `"distr"`, `"lp"`, and `"crank"`.
#'
#' @section Fields:
#' See [Measure].
#'
#' @section Methods:
#' See [Measure].
#'
#' @family Measure
#' @seealso Example survival measures: [`surv.graf`][MeasureSurvGraf],
#' [`surv.harrellC`][MeasureSurvHarrellC].
#' @export
MeasureSurv = R6Class("MeasureSurv", inherit = Measure, cloneable = FALSE,
                      public = list(
                        initialize = function(id, range, minimize = NA, aggregator = NULL, properties = character(), predict_type = "distr", task_properties = character(), packages = character()) {
                          super$initialize(id, task_type = "surv", range = range, minimize = minimize, aggregator = aggregator,
                                           properties = properties, predict_type = predict_type, task_properties = task_properties, packages = packages)
                        }
                      )
)
