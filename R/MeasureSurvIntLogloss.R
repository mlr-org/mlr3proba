#' @title Integrated Logloss
#'
#' @usage NULL
#' @aliases mlr_measures_surv.loglossint
#' @format [R6::R6Class()] inheriting from [MeasureSurvLogloss]/[MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvLoglossInt$new()
#' mlr_measures$get("surv.loglossint")
#' msr("surv.loglossint")
#' ```
#'
#' @description
#' Calculates the integrated cross-entropy, or logarithmic, loss.
#'
#' @details
#' The integrated logloss, in the context of probabilistic predictions, is defined as the classical
#' logloss for a given individual, integrated over all time-points.
#'
#' @template seealso_measure
#' @export
MeasureSurvLoglossInt = R6::R6Class("MeasureSurvLoglossInt",
  inherit = MeasureSurvLogloss,
  public = list(
    initialize = function(eps = 1e-15) {
      super$initialize(eps, id = "surv.loglossint")
    },

    score_internal = function(prediction, ...) {
        mean(integrated_logloss(prediction$truth, prediction$distr, eps = self$eps))
      }
  )
)


