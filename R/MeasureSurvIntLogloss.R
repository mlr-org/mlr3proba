#' @template surv_measure
#' @templateVar title Integrated Log loss
#' @templateVar inherit `MeasureSurvIntegrated`/[MeasureSurv]
#' @templateVar fullname MeasureSurvIntLogloss
#' @templateVar pars integrated = TRUE, times, eps = 1e-15
#' @templateVar int_par TRUE
#' @templateVar times_par TRUE
#' @templateVar eps_par TRUE
#'
#' @description
#' Calculates the integrated logarithmic (log), loss, aka integrated cross entropy.
#'
#' For an individual who dies at time \eqn{t}, with predicted Survival function, \eqn{S}, the
#' probabilistic log loss at time \eqn{t^*}{t*} is given by
#' \deqn{L(S,t|t^*) = - [log(1 - S(t^*))I(t \le t^*, \delta = 1)(1/G(t))] - [log(S(t^*))I(t > t^*)(1/G(t^*))]}{L(S,t|t*) = - [log(1 - S(t*))I(t \le t*, \delta = 1)(1/G(t))] - [log(S(t*))I(t > t*)(1/G(t*))]}
#' where \eqn{G} is the Kaplan-Meier estimate of the censoring distribution.
#'
#' @template measure_integrated
#'
#' @section Fields:
#' As well as
#' * eps :: numeric(1) \cr
#' Very small number to set zero-valued predicted probabilities to, in order to prevent errors in log(0) calculation.
#'
#' @references
#' Graf, E., Schmoor, C., Sauerbrei, W. and Schumacher, M. (1999).\cr
#' Assessment and comparison of prognostic classification schemes for survival data.\cr
#' Statistics in Medicine, 18(17), 2529-2545.\cr
#' \doi{10.1002/(SICI)1097-0258(19990915/30)18:17/18<2529::AID-SIM274>3.0.CO;2-5}
#'
#' @family Probabilistic survival measures
#' @export
MeasureSurvIntLogloss = R6::R6Class("MeasureSurvIntLogloss",
  inherit = MeasureSurvIntegrated,
  public = list(
    initialize = function(integrated = TRUE, times, eps = 1e-15) {
      super$initialize(
        integrated = integrated,
        times = times,
        id = "surv.intlogloss",
        range = c(0, Inf),
        minimize = TRUE,
        packages = "distr6",
        predict_type = "distr",
        properties = character()
      )

      assertNumeric(eps)
      private$.eps <- eps
    },

    score_internal = function(prediction, ...) {
      integrated_score(score = weighted_logloss(truth = prediction$truth,
                                             distribution = prediction$distr,
                                             times = self$times,
                                             eps = self$eps),
                    integrated = self$integrated)
    }
  ),

  active = list(
    eps = function(eps){
      if(missing(eps))
        return(private$.eps)
      else {
        assertNumeric(eps)
        private$.eps <- eps
      }
    }
  ),

  private = list(
    .eps = numeric(0)
  )
)
