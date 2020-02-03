#' @template surv_measure
#' @templateVar title Mean Absolute Error
#' @templateVar inherit [MeasureSurv]
#' @templateVar fullname MeasureSurvMAE
#'
#' @description
#' Calculates the mean absolute error (MAE).
#'
#' The MAE is defined by
#' \deqn{\frac{1}{n} \sum |t - \hat{t}|}{1/n \sum |t - t*|}
#' where \eqn{t} is the true value and \eqn{\hat{t}}{t*} is the prediction.
#'
#' Censored observations in the test set are ignored.
#'
#' @family response survival measures
#' @export
MeasureSurvMAE = R6::R6Class("MeasureSurvMAE",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.mae",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "response"
      )
    },

    score_internal = function(prediction, ...) {
      mean(surv_mae(prediction$truth, prediction$response)$mae)
    }
  )
)
