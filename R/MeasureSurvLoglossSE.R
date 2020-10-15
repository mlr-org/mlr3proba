#' @template surv_measure
#' @templateVar title Standard Error of Log loss
#' @templateVar fullname MeasureSurvLoglossSE
#'
#' @template param_eps
#'
#' @description
#' Calculates the standard error of [MeasureSurvLogloss].
#'
#' The standard error of the Logloss, L, is approximated via,
#' \deqn{se(L) = sd(L)/\sqrt{N}}{se(L) = sd(L)/\sqrt N}
#' where \eqn{N} are the number of observations in the test set, and \eqn{sd} is the standard
#' deviation.
#'
#' Censored observations in the test set are ignored.
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvLoglossSE = R6::R6Class("MeasureSurvLoglossSE",
  inherit = MeasureSurvLogloss,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(eps = 1e-15) {
      warning('MeasureSurvLoglossSE is now deprecated, use msr("surv.logloss", se = TRUE) instead.')
      super$initialize(eps, se = TRUE)
      self$man = "mlr3proba::mlr_measures_surv.logloss_se"
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      ll = surv_logloss(prediction$truth, prediction$distr, self$eps)

      sd(ll) / sqrt(length(ll))
    }
  )
)
