#' @template surv_measure
#' @templateVar title Log loss
#' @templateVar fullname MeasureSurvLogloss
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
#' @template param_id
#' @template param_eps
#' @template field_eps
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvLogloss = R6::R6Class("MeasureSurvLogloss",
  inherit = MeasureSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
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
    .eps = numeric(0),
    .score = function(prediction, ...) {
      mean(surv_logloss(prediction$truth, prediction$distr, self$eps))
    }
  )
)
