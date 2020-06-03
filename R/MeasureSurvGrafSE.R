#' @template surv_measure
#' @templateVar title Standard Error of Integrated Graf Score
#' @templateVar fullname MeasureSurvGrafSE
#'
#' @description
#' Calculates the standard error of [MeasureSurvGraf].
#'
#' @template learner_integratedSE
#' @template param_integrated
#' @template param_times
#'
#' @references
#' \cite{mlr3proba}{graf_1999}
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvGrafSE = R6::R6Class("MeasureSurvGrafSE",
  inherit = MeasureSurvIntegrated,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, times) {
      super$initialize(
        integrated = integrated,
        times = times,
        id = "surv.grafSE",
        range = c(0, Inf),
        minimize = TRUE,
        packages = "distr6",
        predict_type = "distr",
        properties = character()
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
