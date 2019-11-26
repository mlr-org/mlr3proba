#' @title Standard Error of Integrated Logloss
#'
#' @usage NULL
#' @aliases mlr_measures_surv.loglossintSE
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
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
    inherit = MeasureSurv,
    public = list(
      initialize = function() {
        super$initialize(
          id = "surv.loglossintSE",
          range = c(0, Inf),
          minimize = TRUE,
          predict_type = "distr"
        )
      },

      score_internal = function(prediction, ...) {
        ll = integrated_logloss(prediction$truth, prediction$distr)

        sd(ll)/sqrt(length(ll))
      }
    )
)
