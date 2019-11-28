#' @template surv_measure
#' @templateVar title Log loss
#' @templateVar inherit [MeasureSurv]
#' @templateVar fullname MeasureSurvLogloss
#' @templateVar shortname surv.logloss
#' @description
#' Calculates the cross-entropy, or logarithmic (log), loss.
#'
#' The logloss, in the context of probabilistic predictions, is defined as the negative log-likelihood
#' evaluated at the observed death-time,
#' \deqn{L(f, t) = -log(f(t))}
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
      if(is.missing(eps))
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
