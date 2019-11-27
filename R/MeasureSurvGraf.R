#' @title Graf Score
#'
#' @usage NULL
#' @aliases mlr_measures_surv.graf
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvGraf$new()
#' mlr_measures$get("surv.graf")
#' msr("surv.graf")
#' ```
#'
#' @description
#' Calculates the Graf score, aka integrated Brier survival score or squared loss.
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
MeasureSurvGraf = R6::R6Class("MeasureSurvGraf",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.graf",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr"
      )
    },

    score_internal = function(prediction, ...) {
      mean(graf(prediction$truth, prediction$distr))
    }
  )
)

graf = function(truth, distribution) {
  # unweighted graf score at time t* as G(t*) = (I(t > t*) - S(t*))^2
  graf = function(alive, distribution) (alive - transpose(1 - distribution$cdf(unique_times)))^2

  weighted_survival_score(truth, distribution, graf)
}

