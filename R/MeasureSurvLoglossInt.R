#' @template surv_measure
#' @templateVar title Integrated Log loss
#' @templateVar inherit [MeasureSurvLogloss]/[MeasureSurv]
#' @templateVar fullname MeasureSurvLoglossInt
#' @templateVar shortname surv.loglossint
#' @templateVar pars integrated = TRUE, times, eps = 1e-15
#' @templateVar int_par TRUE
#' @templateVar times_par TRUE
#' @templateVar eps_par TRUE
#'
#' @description
#' Calculates the integrated cross-entropy, or logarithmic (log), loss (ILL).
#'
#' For an individual who dies at time \eqn{t}, with predicted Survival function, \eqn{S}, the
#' (unweighted) probabilistic log loss at time t* is given by
#' \deqn{L(S, t*) = -log(1 - S(t*)) * (I(t \le t*) - log(S(t*)) * I(t > t*)}
#'
#' To account for censoring a weighted version is defined by
#' \deqn{L(S, t*) = -log(1 - S(t*)) * I(t \le t*, \delta = 1) * (1/G(t)) - log(S(t*)) * I(t > t*) * (1/G(t*))}
#'
#' As only a finite number of test points are given, an approximation to the ILL can be made by taking
#' the average over all, \eqn{M}, unique discrete time-points given in the test data,
#' \deqn{ILL(S) = 1/M \Sigma_t ILL(S, t)}
#'
#' Finally the sample mean is taken to return a single score for all \eqn{N} observations,
#' \deqn{ILL(pred) = 1/N \Sigma_i ILL(S_i)}
#'
#'
#' @references
#' Graf, E., Schmoor, C., Sauerbrei, W. and Schumacher, M. (1999).\cr
#' Assessment and comparison of prognostic classification schemes for survival data.\cr
#' Statistics in Medicine, 18(17), 2529-2545.\cr
#' \doi{10.1002/(SICI)1097-0258(19990915/30)18:17/18<2529::AID-SIM274>3.0.CO;2-5}
#'
#' @family Probabilistic survival measures
#' @export
MeasureSurvLoglossInt = R6::R6Class("MeasureSurvLoglossInt",
  inherit = MeasureSurvLogloss,
  public = list(
    initialize = function(integrated = TRUE, times, eps = 1e-15) {
      super$initialize(eps, id = "surv.loglossint")

      assertFlag(integrated)
      private$.integrated = integrated

      if (!integrated & !missing(times)) {
        assertNumeric(times)
        private$.times = times
      }
    },

    score_internal = function(prediction, ...) {
      integrated_score(score = weighted_logloss(prediction$truth, prediction$distr, eps = self$eps),
                       integrated = self$integrated,
                       times = self$times
      )
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
