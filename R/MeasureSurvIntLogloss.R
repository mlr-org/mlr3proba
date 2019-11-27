#' @title Integrated Logloss
#'
#' @usage NULL
#' @aliases mlr_measures_surv.loglossint
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
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
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.loglossint",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr"
        )
      },

    score_internal = function(prediction, eps = 1e-15, ...) {
        mean(integrated_logloss(prediction$truth, prediction$distr, eps = eps))
      }
  )
)

integrated_logloss = function(truth, distribution, eps = 1e-15){
  # unweighted logloss score at time t* as L(t*) = (I(t > t*) - S(t*))^2
  logloss = function(alive, distribution, eps){
    # if a patient is alive at t then find the survival, otherwise find cdf
    ll = (surv * alive) + ((1 - surv) * (1 - alive))
    # set prediction to be very small but non-zero then find negative log
    ll[ll == 0] = eps

    -log(ll)
  }

  weighted_survival_score(truth, distribution, logloss, eps = eps)
}
