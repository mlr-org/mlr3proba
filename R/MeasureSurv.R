#' @title Survival Measure
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3::Measure].
#'
#' @description
#' This measure specializes [mlr3::Measure] for classification problems.
#' Predefined measures can be found in the [mlr3misc::Dictionary] [mlr3::mlr_measures].
#'
#' The `task_type` is set to `"classif"`.
#'
#' @section Construction:
#' ```
#' m = MeasureSurv$new(id, range, minimize, predict_type = "risk",
#'      task_properties = character(0L), na_score = FALSE, packages = character(0L))
#' ```
#' For a description of the arguments, see [mlr3::Measure].
#' The `task_type` is set to `"surv"`.
#' Currently only possible value for `predict_types` is "risk".
#'
#' @section Fields:
#' See [Measure].
#'
#' @section Methods:
#' See [Measure].
#'
#' @family Measure
#' @seealso Example survival measure: [`surv.harrells_c`][mlr_measures_surv.harrells_c].
#' @export
MeasureSurv = R6Class("MeasureSurv", inherit = Measure, cloneable = FALSE,
  public = list(
    initialize = function(id, range, minimize = NA, aggregator = NULL, properties = character(), predict_type = "risk", task_properties = character(0L), na_score = FALSE, packages = character(0L)) {
      super$initialize(id, task_type = "surv", range = range, minimize = minimize, aggregator = aggregator,
        properties = properties, predict_type = predict_type, task_properties = task_properties, na_score = na_score, packages = packages)
    }
  )
)
