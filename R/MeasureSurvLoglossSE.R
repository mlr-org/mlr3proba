#' @title Standard Error of Logloss
#'
#' @usage NULL
#' @aliases mlr_measures_surv.loglossSE
#' @format [R6::R6Class()] inheriting from [MeasureSurvLogloss]/[MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvLoglossSESE$new()
#' mlr_measures$get("surv.loglossSE")
#' msr("surv.loglossSE")
#' ```
#'
#' @description
#' Calculates the standard error of [MeasureSurvLogloss].
#'
#' @template seealso_measure
#' @export
MeasureSurvLoglossSE = R6::R6Class("MeasureSurvLoglossSE",
    inherit = MeasureSurvLogloss,
    public = list(
      initialize = function(eps = 1e-15) {
        super$initialize(eps, id = "surv.loglossSE")
      },

      score_internal = function(prediction, ...) {
        ll = surv_logloss(prediction$truth, prediction$distr, self$eps)

        sd(ll)/sqrt(length(ll))
      }
    )
)
