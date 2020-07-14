#' @template dens_measure
#' @templateVar title Log loss
#' @templateVar inherit [MeasureDens]
#' @templateVar fullname MeasureDensLogloss
#' @templateVar pars eps = 1e-15
#' @templateVar eps_par TRUE
#'
#' @template param_eps
#'
#' @description
#' Calculates the cross-entropy, or logarithmic (log), loss.
#'
#' The logloss, in the context of probabilistic predictions, is defined as the negative log
#' probability density function, \eqn{f}, evaluated at the observed value, \eqn{y},
#' \deqn{L(f, y) = -\log(f(y))}{L(f, y) = -log(f(y))}
#'
#' @family Density estimation measures
#' @export
MeasureDensLogloss = R6::R6Class("MeasureDensLogloss",
  inherit = MeasureDens,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(eps = 1e-15) {
      super$initialize(
        id = "dens.logloss",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "pdf",
        man = "mlr3proba::mlr_measures_dens.logloss"
      )

      assertNumeric(eps)
      private$.eps = eps
    }
  ),

  active = list(
    #' @field eps
    #' Returns `eps` parameter, see `initialize`.
    eps = function(eps) {
      if (missing(eps)) {
        return(private$.eps)
      } else {
        assertNumeric(eps)
        private$.eps = eps
      }
    }
  ),

  private = list(
    .eps = numeric(0),
    .score = function(prediction, ...) {
      pdf = prediction$pdf
      pdf[pdf == 0] = self$eps
      return(mean(-log(pdf)))
    }
  )
)
