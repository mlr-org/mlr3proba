#' @title Density Measure
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [mlr3::Measure].
#'
#' @description
#' This measure specializes [mlr3::Measure] for density estimation problems.
#' Predefined measures can be found in the [mlr3misc::Dictionary] [mlr3::mlr_measures].
#'
#' @section Construction:
#' ```
#' MeasureDens$new(id, range, minimize = NA, aggregator = NULL, properties = character(),
#' predict_type = "pdf", task_properties = character(0L), packages = character(0L))
#' ```
#' For a description of the arguments, see [mlr3::Measure].
#' The `task_type` is set to `"dens"`.
#' Possible values for `predict_type` are `"pdf"` and `"cdf"`.
#'
#' @section Fields:
#' See [Measure].
#'
#' @section Methods:
#' See [Measure].
#'
#' @family Measure
#' @export
MeasureDens = R6Class("MeasureDens", inherit = Measure, cloneable = FALSE,
  public = list(
    initialize = function(id, range, minimize = NA, aggregator = NULL, properties = character(), predict_type = "pdf", task_properties = character(0L), packages = character(0L)) {
      super$initialize(id, task_type = "dens", range = range, minimize = minimize, aggregator = aggregator,
        properties = properties, predict_type = predict_type, task_properties = task_properties, packages = packages)
    }
  )
)
