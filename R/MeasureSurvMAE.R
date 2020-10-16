#' @template surv_measure
#' @templateVar title Mean Absolute Error
#' @templateVar fullname MeasureSurvMAE
#'
#' @template param_se
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
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(se = FALSE) {
      super$initialize(
        id = ifelse(se, "surv.mae_se", "surv.mae"),
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "response",
        man = "mlr3proba::mlr_measures_surv.mae",
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
        return(surv_mae(prediction$truth, prediction$response)$se)
      } else {
        return(mean(surv_mae(prediction$truth, prediction$response)$mae))
      }
    }
  )
)
