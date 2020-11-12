#' @template surv_measure
#' @templateVar title Integrated Graf Score
#' @templateVar fullname MeasureSurvGraf
#'
#' @aliases MeasureSurvBrier mlr_measures_surv.brier
#'
#' @description
#' Calculates the Integrated Graf Score, aka integrated Brier score or squared loss.
#'
#' For an individual who dies at time \eqn{t}, with predicted Survival function, \eqn{S}, the
#' Graf Score at time \eqn{t^*}{t*} is given by
#' \deqn{L(S,t|t^*) = [(S(t^*)^2)I(t \le t^*, \delta = 1)(1/G(t))] + [((1 - S(t^*))^2)I(t > t^*)(1/G(t^*))]}{L(S,t|t*) = [(S(t*)^2)I(t \le t*, \delta = 1)(1/G(t))] + [((1 - S(t*))^2)I(t > t*)(1/G(t*))]} # nolint
#' where \eqn{G} is the Kaplan-Meier estimate of the censoring distribution.
#'
#' Note: If comparing the integrated graf score to other packages, e.g. \CRANpkg{pec}, then
#' `method = 2` should be used. However the results may still be very slightly different as
#' this package uses `survfit` to estimate the censoring distribution, in line with the Graf 1999
#' paper; whereas some other packages use `prodlim` with `reverse = TRUE` (meaning Kaplan-Meier is
#' not used).
#'
#' @template measure_integrated
#' @template param_integrated
#' @template param_times
#' @template param_method
#' @template param_se
#'
#' @references
#' `r format_bib("graf_1999")`
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvGraf = R6::R6Class("MeasureSurvGraf",
  inherit = MeasureSurvIntegrated,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, times, method = 2, se = FALSE) {
      super$initialize(
        integrated = integrated,
        times = times,
        method = method,
        id = ifelse(se, "surv.graf_se", "surv.graf"),
        range = c(0, Inf),
        minimize = TRUE,
        packages = character(),
        predict_type = "distr",
        man = "mlr3proba::mlr_measures_surv.graf",
      )

      private$.se = assertFlag(se)
    }
  ),

  active = list(
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
    .se = FALSE,
    .score = function(prediction, ...) {
      if (self$se) {
        return(
          integrated_se(score = weighted_survival_score("graf",
                                                        truth = prediction$truth,
                                                        distribution = prediction$distr,
                                                        times = self$times),
                        integrated = self$integrated)
        )
      } else {
        return(
          integrated_score(score = weighted_survival_score("graf",
                                                           truth = prediction$truth,
                                                           distribution = prediction$distr,
                                                           times = self$times),
                           integrated = self$integrated,
                           method = self$method)
        )
      }
    }
  )
)
