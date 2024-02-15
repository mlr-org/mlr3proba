#' @template regr_measure
#' @templateVar title Log Loss
#' @templateVar inherit [MeasureRegr]
#' @templateVar fullname MeasureRegrLogloss
#' @templateVar eps 1e-15
#' @template param_eps
#'
#' @description
#' Calculates the cross-entropy, or logarithmic (log), loss.
#'
#' @details
#' The Log Loss, in the context of probabilistic predictions, is defined as the negative log
#' probability density function, \eqn{f}, evaluated at the observed value, \eqn{y},
#' \deqn{L(f, y) = -\log(f(y))}{L(f, y) = -log(f(y))}
#'
#' @export
MeasureRegrLogloss = R6::R6Class("MeasureRegrLogloss",
  inherit = MeasureRegr,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        eps = p_dbl(0, 1, default = 1e-15)
      )
      ps$values$eps = 1e-15

      super$initialize(
        id = "regr.logloss",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr",
        man = "mlr3proba::mlr_measures_regr.logloss",
        label = "Log Loss",
        param_set = ps
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      distr = prediction$distr
      truth = prediction$truth

      if (inherits(distr, c("Matdist", "Arrdist"))) {
        pdf = diag(distr$pdf(truth))
      } else {
        pdf = as.numeric(distr$pdf(data = matrix(truth, nrow = 1)))
      }

      pdf[pdf == 0] = self$param_set$values$eps
      mean(-log(pdf))
    }
  )
)

register_measure("regr.logloss", MeasureRegrLogloss)
