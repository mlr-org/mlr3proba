#' @template surv_measure
#' @templateVar title Standard Error of Integrated Log loss
#' @templateVar inherit [MeasureSurvLogloss]/[MeasureSurv]
#' @templateVar fullname MeasureSurvLoglossIntSE
#' @templateVar shortname surv.loglossintSE
#' @templateVar pars integrated = TRUE, times, eps = 1e-15
#' @templateVar int_par TRUE
#' @templateVar times_par TRUE
#' @templateVar eps_par TRUE
#'
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
      initialize = function(integrated = TRUE, times, eps = 1e-15) {
        super$initialize(eps, id = "surv.loglossintSE")

        assertFlag(integrated)
        private$.integrated = integrated

        if (!integrated & !missing(times)) {
          assertNumeric(times)
          private$.times = times
        }
      },

      score_internal = function(prediction, ...) {
        integrated_se(score = weighted_logloss(prediction$truth, prediction$distr, self$eps),
                      integrated = self$integrated,
                      times = self$times)
      }
    ),

    active = list(
      integrated = function(integrated) {
        if (missing(integrated)) {
          return(private$.integrated)
        } else {
          assertFlag(integrated)
          private$.integrated = integrated
        }
      },
      times = function(times) {
        if (missing(times)) {
          return(private$.times)
        } else {
          assertNumeric(times)
          private$.times = times
        }
      }
    ),

    private = list(
      .times = numeric(),
      .integrated = logical()
    )
)
