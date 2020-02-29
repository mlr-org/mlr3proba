#' @template dens_measure
#' @templateVar title Log loss
#' @templateVar inherit [MeasureDens]
#' @templateVar fullname MeasureDensLogloss
#' @templateVar pars eps = 1e-15
#' @templateVar eps_par TRUE
#'
#' @description
#' Calculates the cross-entropy, or logarithmic (log), loss.
#'
#' The logloss, in the context of probabilistic predictions, is defined as the negative log probability
#' density function, \eqn{f}, evaluated at the observed value, \eqn{y},
#' \deqn{L(f, y) = -log(f(y))}
#'
#' @section Fields:
#' As well as
#' * eps :: numeric(1) \cr
#' Very small number to set zero-valued predicted probabilities to, in order to prevent errors in log(0) calculation.
#'
#' @family Density estimation measures
#' @export
MeasureDensLogloss = R6::R6Class("MeasureDensLogloss",
  inherit = MeasureDens,
  public = list(
    initialize = function(eps = 1e-15) {
      super$initialize(
        id = "dens.logloss",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "pdf"
      )

      assertNumeric(eps)
      private$.eps <- eps
    },

    score_internal = function(prediction, ...) {
      pdf = prediction$pdf
      pdf[pdf == 0] = self$eps
      return(mean(-log(pdf)))
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
