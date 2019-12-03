#' @template surv_measure
#' @templateVar title Integrated Graf Score
#' @templateVar inherit `MeasureSurvIntegrated`/[MeasureSurv]
#' @templateVar fullname MeasureSurvGraf
#' @templateVar pars integrated = TRUE, times
#' @templateVar int_par TRUE
#' @templateVar times_par TRUE
#'
#' @aliases MeasureSurvBrier mlr_measures_surv.brier
#'
#' @description
#' Calculates the Integrated Graf Score, aka integrated Brier score or squared loss.
#'
#' For an individual who dies at time \eqn{t}, with predicted Survival function, \eqn{S}, the
#' Graf Score at time \eqn{t^*}{t*} is given by
#' \deqn{L(S,t|t^*) = [(S(t^*)^2)I(t \le t^*, \delta = 1)(1/G(t))] + [((1 - S(t^*))^2)I(t > t^*)(1/G(t^*))]}{L(S,t|t*) = [(S(t*)^2)I(t \le t*, \delta = 1)(1/G(t))] + [((1 - S(t*))^2)I(t > t*)(1/G(t*))]}
#' where \eqn{G} is the Kaplan-Meier estimate of the censoring distribution.
#'
#' @template measure_integrated
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
  inherit = MeasureSurvIntegrated,
  public = list(
    initialize = function(integrated = TRUE, times) {
      super$initialize(
        integrated = integrated,
        times = times,
        id = "surv.graf",
        range = c(0, Inf),
        minimize = TRUE,
        packages = "distr6",
        predict_type = "distr",
        properties = character()
      )
    },

    score_internal = function(prediction, ...) {
      integrated_score(score = weighted_graf(truth = prediction$truth,
                                             distribution = prediction$distr,
                                             times = self$times),
                       integrated = self$integrated)
    }
  )
)
