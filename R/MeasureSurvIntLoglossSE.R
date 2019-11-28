#' @template surv_measure
#' @templateVar title Standard Error of Integrated Log loss
#' @templateVar inherit [MeasureSurvLogloss]/[MeasureSurv]
#' @templateVar fullname MeasureSurvLoglossIntSE
#' @templateVar shortname surv.loglossintSE
#' @description
#' Calculates the standard error of [MeasureSurvLoglossInt].
#'
#' The standard error is approximated using Binomial approxiation. For \eqn{N} observations in the
#' test set, the standard eror is given by
#' \deqn{ILL_SE = sd(ILL(S))/sqrt(N)}
#'
#' @references
#' Graf, E., Schmoor, C., Sauerbrei, W. and Schumacher, M. (1999).\cr
#' Assessment and comparison of prognostic classification schemes for survival data.\cr
#' Statistics in Medicine, 18(17), 2529-2545.\cr
#' \doi{10.1002/(SICI)1097-0258(19990915/30)18:17/18<2529::AID-SIM274>3.0.CO;2-5}
#'
#' @family Probabilistic survival measures
#' @export
MeasureSurvLoglossIntSE = R6::R6Class("MeasureSurvLoglossIntSE",
    inherit = MeasureSurvLogloss,
    public = list(
      initialize = function(eps = 1e-15) {
        super$initialize(eps, id = "surv.loglossintSE")
      },

      score_internal = function(prediction, ...) {
        ll = integrated_logloss(prediction$truth, prediction$distr, self$eps)

        sd(ll)/sqrt(length(ll))
      }
    )
)
