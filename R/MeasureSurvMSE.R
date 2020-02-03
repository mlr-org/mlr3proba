#' @template surv_measure
#' @templateVar title Mean Squared Error
#' @templateVar inherit [MeasureSurv]
#' @templateVar fullname MeasureSurvMSE
#'
#' @description
#' Calculates the mean squared error (MSE).
#'
#' The MSE is defined by
#' \deqn{\frac{1}{n} \sum ((t - \hat{t})^2)}{1/n \sum ((t - t*)^2)}
#' where \eqn{t} is the true value and \eqn{\hat{t}}{t*} is the prediction.
#'
#' Censored observations in the test set are ignored.
#'
#' @family response survival measures
#' @export
MeasureSurvMSE = R6::R6Class("MeasureSurvMSE",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.mse",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "response"
      )
    },

    score_internal = function(prediction, ...) {
      mean(surv_mse(prediction$truth, prediction$response)$mse)
    }
  )
)
