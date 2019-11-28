#' @template surv_measure
#' @templateVar title Integrated Graf Score
#' @templateVar inherit [MeasureSurv]
#' @templateVar fullname MeasureSurvGraf
#' @templateVar shortname surv.graf
#' @aliases MeasureSurvBrier mlr_measures_surv.brier
#' @description
#' Calculates the Integrated Graf Score (IGS), aka integrated Brier survival score or squared loss.
#' An approximation to the IGS is calculated by taking the mean over all time-points in the test set.
#'
#' For an individual who dies at time \eqn{t}, with predicted Survival function, \eqn{S}, the
#' (unweighted) Graf Score at time t* is given by
#' \deqn{G(S, t*) = (I(t > t*) - S(t*))^2}
#'
#' To account for censoring a weighted version is defined by
#' \deqn{G(S, t*) = S(t*)^2 * I(t \le t*, \delta = 1) * (1/G(t)) + (1 - S(t*))^2 * I(t > t*) * (1/G(t*))}
#'
#' As only a finite number of test points are given, an approximation to the IGS can be made by taking
#' the average over all, \eqn{M}, unique discrete time-points given in the test data,
#' \deqn{IGS(S) = 1/M \Sigma_t G(S, t)}
#'
#' Finally the sample mean is taken to return a single score for all \eqn{N} observations,
#' \deqn{IGS(pred) = 1/N \Sigma_i IGS(S_i)}
#'
#'
#' @references
#' Graf, E., Schmoor, C., Sauerbrei, W. and Schumacher, M. (1999).\cr
#' Assessment and comparison of prognostic classification schemes for survival data.\cr
#' Statistics in Medicine, 18(17), 2529-2545.\cr
#' \doi{10.1002/(SICI)1097-0258(19990915/30)18:17/18<2529::AID-SIM274>3.0.CO;2-5}
#'
#' @family Probabilistic survival measures
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
      mean(integrated_graf(prediction$truth, prediction$distr))
    }
  )
)

