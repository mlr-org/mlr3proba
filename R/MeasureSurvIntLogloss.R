#' @template surv_measure
#' @templateVar title Integrated Log loss
#' @templateVar fullname MeasureSurvIntLogloss
#'
#' @description
#' Calculates the integrated logarithmic (log), loss, aka integrated cross entropy.
#'
#' For an individual who dies at time \eqn{t}, with predicted Survival function, \eqn{S}, the
#' probabilistic log loss at time \eqn{t^*}{t*} is given by
#' \deqn{L(S,t|t^*) = - [log(1 - S(t^*))I(t \le t^*, \delta = 1)(1/G(t))] - [log(S(t^*))I(t > t^*)(1/G(t^*))]}{L(S,t|t*) = - [log(1 - S(t*))I(t \le t*, \delta = 1)(1/G(t))] - [log(S(t*))I(t > t*)(1/G(t*))]} # nolint
#' where \eqn{G} is the Kaplan-Meier estimate of the censoring distribution.
#'
#' @template measure_integrated
#' @template param_integrated
#' @template param_times
#' @template param_eps
#' @template field_eps
#' @template param_method
#' @template param_se
#'
#' @references
#' `r format_bib("graf_1999")`
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvIntLogloss = R6::R6Class("MeasureSurvIntLogloss",
  inherit = MeasureSurvIntegrated,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, times, eps = 1e-15, method = 2, se = FALSE) {
      super$initialize(
        integrated = integrated,
        times = times,
        method = method,
        id = ifelse(se, "surv.intlogloss_se", "surv.intlogloss"),
        range = c(0, Inf),
        minimize = TRUE,
        packages = "distr6",
        predict_type = "distr",
        man = "mlr3proba::mlr_measures_surv.intlogloss",
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
        return(
          integrated_score(score = weighted_survival_score("intslogloss",
                                                           truth = prediction$truth,
                                                           distribution = prediction$distr,
                                                           times = self$times,
                                                           eps = self$eps),
                           integrated = self$integrated,
                           method = self$method)
        )
      } else {
        return(
          integrated_se(score = weighted_survival_score("intslogloss",
                                                        truth = prediction$truth,
                                                        distribution = prediction$distr,
                                                        times = self$times,
                                                        eps = self$eps),
                        integrated = self$integrated)
        )
      }
    }
  )
)
