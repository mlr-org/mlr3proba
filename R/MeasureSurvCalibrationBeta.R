#' @template surv_measure
#' @templateVar title Van Houwelingen's Beta
#' @templateVar fullname MeasureSurvCalibrationBeta
#'
#' @template param_se
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
#' `r format_bib("vanhouwelingen_2000")`
#'
#' @family calibration survival measures
#' @family lp survival measures
#' @export
MeasureSurvCalibrationBeta = R6Class("MeasureSurvCalibrationBeta",
  inherit = MeasureSurv,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(se = FALSE) {
      super$initialize(
        id = ifelse(se, "surv.calib_beta_se", "surv.calib_beta"),
        range = c(-Inf, Inf),
        minimize = FALSE,
        predict_type = "lp",
        man = "mlr3proba::mlr_measures_surv.calib_beta",
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

      df = data.frame(truth = prediction$truth, lp = prediction$lp)
      fit = try(summary(survival::coxph(truth ~ lp, data = df)), silent = TRUE)
      if (class(fit)[1] == "try-error") {
        return(NA)
      } else {
        if (self$se) {
          return(fit$coefficients[3])
        } else {
          return(fit$coefficients[1])
        }
      }
    }
  )
)
