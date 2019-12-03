#' @template surv_measure
#' @templateVar title Log loss
#' @templateVar inherit [MeasureSurv]
#' @templateVar fullname MeasureSurvLogloss
#' @templateVar shortname surv.logloss
#' @templateVar pars eps = 1e-15
#' @templateVar eps_par TRUE
#'
#' @description
#' Calculates the cross-entropy, or logarithmic (log), loss.
#'
#' The logloss, in the context of probabilistic predictions, is defined as the negative log probability
#' density function, \eqn{f}, evaluated at the observation time, \eqn{t},
#' \deqn{L(f, t) = -log(f(t))}
#'
#' Censored observations in the test set are ignored.
#'
#' @section Fields:
#' As well as
#' * eps :: numeric(1) \cr
#' Very small number to set zero-valued predicted probabilities to, in order to prevent errors in log(0) calculation.
#'
#' @family Probabilistic survival measures
#' @export
MeasureSurvLogloss = R6::R6Class("MeasureSurvLogloss",
  inherit = MeasureSurv,
  public = list(
    initialize = function(eps = 1e-15, id = "surv.logloss") {
      super$initialize(
        id = id,
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr",
        packages = "distr6"
      )

      assertNumeric(eps)
      private$.eps <- eps
    },

    score_internal = function(prediction, ...) {
      mean(surv_logloss(prediction$truth, prediction$distr, self$eps))
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
