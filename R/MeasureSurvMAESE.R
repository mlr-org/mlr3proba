#' @template surv_measure
#' @templateVar title Standard Error of Mean Absolute Error
#' @templateVar fullname MeasureSurvMAESE
#'
#' @description
#' Calculates the standard error of [MeasureSurvMAE].
#'
#' The standard error of the MAE, L, is approximated via
#' \deqn{se(L) = sd(L)/\sqrt{N}}{se(L) = sd(L)/\sqrt N}
#'
#' Censored observations in the test set are ignored.
#'
#' @family response survival measures
#' @export
MeasureSurvMAESE = R6::R6Class("MeasureSurvMAESE",
  inherit = MeasureSurv,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      warning('MeasureSurvMAESE is now deprecated, use msr("surv.mae", se = TRUE) instead.')
      super$initialize(
        id = "surv.maeSE",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "response",
        man = "mlr3proba::mlr_measures_surv.maeSE"
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      surv_mae(prediction$truth, prediction$response)$se
    }
  )
)
