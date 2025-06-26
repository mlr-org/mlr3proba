#' @template surv_measure
#' @templateVar title Mean Absolute Error
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
MeasureSurvMAE = R6Class("MeasureSurvMAE",
  inherit = MeasureSurv,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "surv.mae",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "response",
        label = "Mean Absolute Error",
        man = "mlr3proba::mlr_measures_surv.mae"
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      mean(obs_surv_errors(prediction$truth, prediction$response, method = "abs"))
    }
  )
)

register_measure("surv.mae", MeasureSurvMAE)
