#' @template surv_measure
#' @templateVar title Standard Error of Integrated Graf Score
#' @templateVar fullname MeasureSurvGrafSE
#'
#' @aliases MeasureSurvBrierSE mlr_measures_surv.brier_se
#'
#' @description
#' Calculates the standard error of [MeasureSurvGraf].
#'
#' @template learner_integratedSE
#' @template param_integrated
#' @template param_times
#'
#' @references
#' `r format_bib("graf_1999")`
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
      warning('MeasureSurvGrafSE is now deprecated, use msr("surv.graf", se = TRUE) instead.')
      super$initialize(
        integrated = integrated,
        times = times,
        id = "surv.grafSE",
        range = c(0, Inf),
        minimize = TRUE,
        packages = "distr6",
        predict_type = "distr",
        man = "mlr3proba::mlr_measures_surv.grafSE"
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      integrated_se(score = weighted_survival_score("graf",
                                                    truth = prediction$truth,
                                                    distribution = prediction$distr,
                                                    times = self$times),
                    integrated = self$integrated)
    }
  )
)
