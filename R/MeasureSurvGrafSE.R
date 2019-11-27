#' @title Graf Score
#'
#' @usage NULL
#' @aliases mlr_measures_surv.grafSE
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvGrafSE$new()
#' mlr_measures$get("surv.grafSE")
#' msr("surv.grafSE")
#' ```
#'
#' @description
#' Calculates the standard error of [MeasureSurvGraf].
#'
#' @references
#' Graf, G. (1950).
#' Verification of forecasts expressed in terms of probability.
#' Monthly Weather Review, 78(1), 1-3.
#' \doi{10.1175/1520-0493(1950)078<0001:VOFEIT>2.0.CO;2}
#'
#' Graf, E., Schmoor, C., Sauerbrei, W. and Schumacher, M. (1999).
#' Assessment and comparison of prognostic classification schemes for survival data.
#' Statistics in Medicine, 18(17), 2529-2545.
#' \doi{10.1002/(SICI)1097-0258(19990915/30)18:17/18<2529::AID-SIM274>3.0.CO;2-5}
#'
#'
#' @template seealso_measure
#' @export
MeasureSurvGrafSE = R6::R6Class("MeasureSurvGrafSE",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.grafSE",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr"
      )
    },

    score_internal = function(prediction, ...) {
      graf = integrated_graf(prediction$truth, prediction$distr)

      sd(graf)/sqrt(length(graf))
    }
  )
)
