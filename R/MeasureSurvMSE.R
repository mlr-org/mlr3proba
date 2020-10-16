#' @template surv_measure
#' @templateVar title Mean Squared Error
#' @templateVar fullname MeasureSurvMSE
#'
#' @template param_se
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
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(se = FALSE) {
      super$initialize(
        id = ifelse(se, "surv.mse_se", "surv.mse"),
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "response",
        man = "mlr3proba::mlr_measures_surv.mse",
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
        return(surv_mse(prediction$truth, prediction$response)$se)
      } else {
        return(mean(surv_mse(prediction$truth, prediction$response)$mse))
      }
    }
  )
)
