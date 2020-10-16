#' @template surv_measure
#' @templateVar title Root Mean Squared Error
#' @templateVar fullname MeasureSurvRMSE
#'
#' @template param_se
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
MeasureSurvRMSE = R6::R6Class("MeasureSurvRMSE",
  inherit = MeasureSurv,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(se = FALSE) {
      super$initialize(
        id = ifelse(se, "surv.rmse_se", "surv.rmse"),
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "response",
        man = "mlr3proba::mlr_measures_surv.rmse",
      )

      private$.se = assertFlag(se)
    }
  ),

  active = list(
    #' @field se `(logical(1))` \cr
    #' If `TRUE` returns the standard error of the measure.
    se = function(x) {
      if (!missing(x)) {
        private$.se = assertFlag(x)
      } else {
        return(private$.se)
      }
    }
  ),

  private = list(
    .se = FALSE,
    .score = function(prediction, ...) {
      if (self$se) {
        mse = surv_mse(prediction$truth, prediction$response)
        return(mse$se / (2 * sqrt(mean(mse$mse))))
      } else {
        return(sqrt(mean(surv_mse(prediction$truth, prediction$response)$mse)))
      }
    }
  )
)
