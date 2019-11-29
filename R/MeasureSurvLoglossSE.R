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
#' The standard error is approximated using Binomial approxiation. For \eqn{N} observations in the
#' test set, the standard eror is given by
#' \deqn{LL_SE = sd(LL(S))/sqrt(N)}
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
