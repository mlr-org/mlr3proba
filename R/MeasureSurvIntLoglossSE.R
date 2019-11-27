#' @title Standard Error of Integrated Logloss
#'
#' @usage NULL
#' @aliases mlr_measures_surv.loglossintSE
#' @format [R6::R6Class()] inheriting from [MeasureSurvLogloss]/[MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvLoglossIntSE$new()
#' mlr_measures$get("surv.loglossintSE")
#' msr("surv.loglossintSE")
#' ```
#'
#' @description
#' Calculates the standard error of [MeasureSurvLoglossInt].
#'
#' @template seealso_measure
#' @export
MeasureSurvLoglossIntSE = R6::R6Class("MeasureSurvLoglossIntSE",
    inherit = MeasureSurvLogloss,
    public = list(
      initialize = function(eps = 1e-15) {
        super$initialize(eps, id = "surv.loglossintSE")
      },

      score_internal = function(prediction, ...) {
        ll = integrated_logloss(prediction$truth, prediction$distr, self$eps)

        sd(ll)/sqrt(length(ll))
      }
    )
)
