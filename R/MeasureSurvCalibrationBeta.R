#' @template surv_measure
#' @templateVar title Van Houwelingen's Beta
#' @templateVar fullname MeasureSurvCalibrationBeta
#'
#' @description
#' This calibration method fits the predicted linear predictor from a Cox PH model as the only
#' predictor in a new Cox PH model with the test data as the response.
#' \deqn{h(t|x) = h_0(t)exp(l\beta)}
#' where \eqn{l} is the predicted linear predictor.
#'
#' The model is well calibrated if the estimated \eqn{\beta} coefficient is equal to 1.
#'
#' Assumes fitted model is Cox PH.
#'
#' @references
#' \cite{mlr3proba}{vanhouwelingen_2000}
#'
#' @family calibration survival measures
#' @family lp survival measures
#' @export
MeasureSurvCalibrationBeta = R6Class("MeasureSurvCalibrationBeta",
  inherit = MeasureSurv,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    #' @param se `(logical(1))` \cr
    #' If `TRUE` returns the standard error of the measure.
    initialize = function(se = FALSE) {
      private$.se = assertFlag(se)
      super$initialize(
        id = "surv.calib_beta",
        range = c(-Inf, Inf),
        minimize = FALSE,
        predict_type = "lp",
        man = "mlr3proba::mlr_measures_surv.calib_beta"
      )
    }
  ),

  active = list(
    #' @field se `(logical(1))` \cr
    #' If `TRUE` returns the standard error of the measure.
    se = function(x) {
      if (missing(x)) {
        private$.se = assertFlag(se)
      } else {
        return(private$.se)
      }
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      df = data.frame(truth = prediction$truth, lp = prediction$lp)
      fit = summary(survival::coxph(truth ~ lp, data = df))
      if (private$.se) {
        return(fit$coefficients[3])
      } else {
        return(fit$coefficients[1])
      }
    },
    .se = FALSE
  )
)
