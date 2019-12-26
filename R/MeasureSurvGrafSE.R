#' @template surv_measure
#' @templateVar title Standard Error of Integrated Graf Score
#' @templateVar inherit `MeasureSurvIntegrated`/[MeasureSurv]
#' @templateVar fullname MeasureSurvGrafSE
#' @templateVar pars integrated = TRUE, times
#' @templateVar int_par TRUE
#' @templateVar times_par TRUE
#'
#' @description
#' Calculates the standard error of [MeasureSurvGraf].
#'
#' @template learner_integratedSE
#'
#' @references
#' \cite{mlr3proba}{graf_1999}
#'
#' @family Probabilistic survival measures
#' @export
MeasureSurvGrafSE = R6::R6Class("MeasureSurvGrafSE",
  inherit = MeasureSurvIntegrated,
  public = list(
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
    },

    score_internal = function(prediction, ...) {
      integrated_se(score = weighted_graf(truth = prediction$truth,
                                          distribution = prediction$distr,
                                          times = self$times),
                    integrated = self$integrated)
    }
  )
)
