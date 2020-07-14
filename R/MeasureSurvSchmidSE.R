#' @template surv_measure
#' @templateVar title Standard Error of Integrated Schmid Score
#' @templateVar fullname MeasureSurvSchmidSE
#'
#' @description
#' Calculates the standard error of [MeasureSurvSchmid].
#'
#' @template learner_integratedSE
#' @template param_integrated
#' @template param_times
#'
#' @references
#' \cite{mlr3proba}{schemper_2000}
#' \cite{mlr3proba}{schmid_2011}
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvSchmidSE = R6::R6Class("MeasureSurvSchmidSE",
  inherit = MeasureSurvIntegrated,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, times) {
      super$initialize(
        integrated = integrated,
        times = times,
        id = "surv.schmid_se",
        range = c(0, Inf),
        minimize = TRUE,
        packages = "distr6",
        predict_type = "distr",
        man = "mlr3proba::mlr_measures_surv.schmid_se"
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      integrated_se(
        score = weighted_graf(
          truth = prediction$truth,
          distribution = prediction$distr,
          times = self$times),
        integrated = self$integrated)
    }
  )
)
