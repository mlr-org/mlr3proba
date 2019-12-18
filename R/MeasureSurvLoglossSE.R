#' @template surv_measure
#' @templateVar title Standard Error of Log loss
#' @templateVar inherit [MeasureSurvLogloss]/[MeasureSurv]
#' @templateVar fullname MeasureSurvLoglossSE
#' @templateVar shortname surv.loglossSE
#' @templateVar pars eps = 1e-15
#' @templateVar eps_par TRUE
#'
#' @description
#' Calculates the standard error of [MeasureSurvLogloss].
#'
#' The standard error of the Logloss, L, is approximated via,
#' \deqn{se(L) = sd(L)/\sqrt{N}}{se(L) = sd(L)/\sqrt N}
#' where \eqn{N} are the number of observations in the test set, and \eqn{sd} is the standard deviation.
#'
#' Censored observations in the test set are ignored.
#'
#' @family Probabilistic survival measures
#' @export
MeasureSurvLoglossSE = R6::R6Class("MeasureSurvLoglossSE",
    inherit = MeasureSurvLogloss,
    public = list(
      initialize = function(eps = 1e-15) {
        super$initialize(eps, id = "surv.loglossSE")
      },

      score_internal = function(prediction, ...) {
        ll = surv_logloss(prediction$truth, prediction$distr, self$eps)

        sd(ll)/sqrt(length(ll))
      }
    )
)
