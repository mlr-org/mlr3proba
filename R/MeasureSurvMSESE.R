#' @template surv_measure
#' @templateVar title Standard Error of Mean Squared Error
#' @templateVar fullname MeasureSurvMSESE
#'
#' @description
#' Calculates the standard error of [MeasureSurvMSE].
#'
#' The standard error of the MSE, L, is approximated via
#' \deqn{se(L) = sd(L)/\sqrt{N}}{se(L) = sd(L)/\sqrt N}
#'
#' Censored observations in the test set are ignored.
#'
#' @family response survival measures
#' @export
MeasureSurvMSESE = R6::R6Class("MeasureSurvMSESE",
  inherit = MeasureSurv,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "surv.mseSE",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "response"
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      surv_mse(prediction$truth, prediction$response)$se
    }
  )
)
