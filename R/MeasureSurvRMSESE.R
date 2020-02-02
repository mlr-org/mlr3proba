#' @template surv_measure
#' @templateVar title Standard Error of Root Mean Squared Error
#' @templateVar inherit [MeasureSurv]
#' @templateVar fullname MeasureSurvRMSESE
#'
#' @description
#' Calculates the standard error of [MeasureSurvRMSE].
#'
#' The standard error of the RMSE, L, is approximated via first order approximation by
#' \deqn{sd(RMSE) = Var(\sqrt{MSE})}{sd(RMSE) = Var(\sqrt(MSE))}
#'
#' Censored observations in the test set are ignored.
#'
#' @family response survival measures
#' @export
MeasureSurvRMSESE = R6::R6Class("MeasureSurvRMSESE",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.rmseSE",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "response"
      )
    },

    score_internal = function(prediction, ...) {
      mse = surv_mse(prediction$truth, prediction$response)
      mse$se/(2*sqrt(mean(mse$mse)))
    }
  )
)
