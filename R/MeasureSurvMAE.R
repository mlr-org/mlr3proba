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
    initialize = function() {
      ps = ps(
        se = p_lgl(default = FALSE)
      )
      ps$values$se = FALSE

      super$initialize(
        id = "surv.mae",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "response",
        man = "mlr3proba::mlr_measures_surv.mae",
        param_set = ps
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      if (self$param_set$values$se) {
        return(surv_mae(prediction$truth, prediction$response)$se)
      } else {
        return(mean(surv_mae(prediction$truth, prediction$response)$mae))
      }
    }
  )
)
