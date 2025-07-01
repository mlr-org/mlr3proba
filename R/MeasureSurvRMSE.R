#' @template surv_measure
#' @templateVar title Root Mean Squared Error
#' @templateVar fullname MeasureSurvRMSE
#'
#' @description
#' Calculates the root mean squared error (RMSE).
#'
#' The RMSE is defined by
#' \deqn{\sqrt{\frac{1}{n} \sum ((t - \hat{t})^2)}}{\sqrt(1/n \sum ((t - t*)^2))}
#' where \eqn{t} is the true value and \eqn{\hat{t}}{t*} is the prediction.
#'
#' Censored observations in the test set are ignored.
#'
#' @family response survival measures
#' @export
MeasureSurvRMSE = R6Class("MeasureSurvRMSE",
  inherit = MeasureSurv,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "surv.rmse",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "response",
        label = "Root Mean Squared Error",
        man = "mlr3proba::mlr_measures_surv.rmse",
        properties = "na_score"
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      sqrt(mean(.obs_surv_errors(prediction$truth, prediction$response, method = "squared")))
    }
  )
)

register_measure("surv.rmse", MeasureSurvRMSE)
