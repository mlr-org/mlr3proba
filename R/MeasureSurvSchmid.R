#' @template surv_measure
#' @templateVar title Integrated Schmid Score
#' @templateVar fullname MeasureSurvSchmid
#'
#' @description
#' Calculates the Integrated Schmid Score, aka integrated absolute loss.
#'
#' For an individual who dies at time \eqn{t}, with predicted Survival function, \eqn{S}, the
#' Schmid Score at time \eqn{t^*}{t*} is given by
#' \deqn{L(S,t|t^*) = [(S(t^*))I(t \le t^*, \delta = 1)(1/G(t))] + [((1 - S(t^*)))I(t > t^*)(1/G(t^*))]}{L(S,t|t*) = [(S(t*))I(t \le t*, \delta = 1)(1/G(t))] + [((1 - S(t*)))I(t > t*)(1/G(t*))]} # nolint
#' where \eqn{G} is the Kaplan-Meier estimate of the censoring distribution.
#'
#' @template measure_integrated
#' @template param_integrated
#' @template param_times
#' @template param_method
#' @template param_se
#'
#' @references
#' `r format_bib("schemper_2000", "schmid_2011")`
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvSchmid = R6::R6Class("MeasureSurvSchmid",
  inherit = MeasureSurvIntegrated,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, times, method = 2, se = FALSE) {
      super$initialize(
        integrated = integrated,
        times = times,
        method = method,
        id = ifelse(se, "surv.schmid_se", "surv.schmid"),
        range = c(0, Inf),
        minimize = TRUE,
        packages = "distr6",
        predict_type = "distr",
        man = "mlr3proba::mlr_measures_surv.schmid",
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
    .score = function(prediction, ...) {
      score = weighted_survival_score("schmid",
                                      truth = prediction$truth,
                                      distribution = prediction$distr,
                                      times = self$times)

      if (self$se) {
        return(integrated_se(score = score, integrated = self$integrated))
      } else {
        return(integrated_score(score = score, integrated = self$integrated, method = self$method))
      }
    },

    .se = FALSE
  )
)
