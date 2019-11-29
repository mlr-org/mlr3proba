#' @template surv_measure
#' @templateVar title Standard Error of Integrated Graf Score
#' @templateVar inherit [MeasureSurv]
#' @templateVar fullname MeasureSurvGrafSE
#' @templateVar shortname surv.grafSE
#' @templateVar pars integrated = TRUE, times
#' @templateVar int_par TRUE
#' @templateVar times_par TRUE
#'
#' @description
#' Calculates the standard error of [MeasureSurvGraf].
#'
#' The standard error is approximated using Binomial approxiation. For \eqn{N} observations in the
#' test set, the standard eror is given by
#' \deqn{IGS_SE = sd(IGS(S))/sqrt(N)}
#'
#' @references
#' Graf, E., Schmoor, C., Sauerbrei, W. and Schumacher, M. (1999).\cr
#' Assessment and comparison of prognostic classification schemes for survival data.\cr
#' Statistics in Medicine, 18(17), 2529-2545.\cr
#' \doi{10.1002/(SICI)1097-0258(19990915/30)18:17/18<2529::AID-SIM274>3.0.CO;2-5}
#'
#' @family Probabilistic survival measures
#' @export
MeasureSurvGrafSE = R6::R6Class("MeasureSurvGrafSE",
  inherit = MeasureSurv,
  public = list(
    initialize = function(integrated = TRUE, times) {
      super$initialize(
        id = "surv.grafSE",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr"
      )

      assertFlag(integrated)
      private$.integrated = integrated

      if (!integrated & !missing(times)) {
        assertNumeric(times)
        private$.times = times
      }
    },

    score_internal = function(prediction, ...) {
      integrated_se(score = weighted_graf(prediction$truth, prediction$distr),
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
