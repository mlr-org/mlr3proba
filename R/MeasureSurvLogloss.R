#' @template surv_measure
#' @templateVar title Log loss
#' @templateVar fullname MeasureSurvLogloss
#'
#' @description
#' Calculates the cross-entropy, or logarithmic (log), loss.
#'
#' The logloss, in the context of probabilistic predictions, is defined as the negative log
#' probability density function, \eqn{f}, evaluated at the observation time, \eqn{t},
#' \deqn{L(f, t) = -log(f(t))}
#'
#' The standard error of the Logloss, L, is approximated via,
#' \deqn{se(L) = sd(L)/\sqrt{N}}{se(L) = sd(L)/\sqrt N}
#' where \eqn{N} are the number of observations in the test set, and \eqn{sd} is the standard
#' deviation.
#'
#' Censored observations in the test set are ignored.
#'
#' @template param_id
#' @template param_eps
#' @template field_eps
#' @template param_se
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvLogloss = R6::R6Class("MeasureSurvLogloss",
  inherit = MeasureSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "surv.logloss", eps = 1e-15, se = FALSE) {
      super$initialize(
        id = id,
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr",
        packages = "distr6",
        man = "mlr3proba::mlr_measures_surv.logloss",
      )

      private$.eps = assertNumeric(eps)
      private$.se = assertFlag(se)
    }
  ),

  active = list(
    eps = function(eps) {
      if (missing(eps)) {
        return(private$.eps)
      } else {
        assertNumeric(eps)
        private$.eps = eps
      }
    },

    #' @field se `(logical(1))` \cr
    #' If `TRUE` returns the standard error of the measure.
    se = function(x) {
      if (!missing(x)) {
        private$.se = assertFlag(x)
      } else {
        return(private$.se)
      }
    }
  ),

  private = list(
    .eps = numeric(0),
    .se = FALSE,
    .score = function(prediction, ...) {
      if (self$se) {
        ll = surv_logloss(prediction$truth, prediction$distr, self$eps)
        return(sd(ll) / sqrt(length(ll)))
      } else {
       return(mean(surv_logloss(prediction$truth, prediction$distr, self$eps)))
      }
    }
  )
)
