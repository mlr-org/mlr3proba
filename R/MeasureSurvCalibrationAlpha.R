#' @template surv_measure
#' @templateVar title Van Houwelingen's Alpha
#' @templateVar fullname MeasureSurvCalibrationAlpha
#'
#' @template param_se
#'
#' @description
#' This calibration method is defined by estimating
#' \deqn{\alpha = \sum \delta_i / \sum H_i(t_i)}
#' where \eqn{\delta} is the observed censoring indicator from the test data, \eqn{H_i} is the
#' predicted cumulative hazard, and \eqn{t_i} is the observed survival time.
#'
#' The standard error is given by
#' \deqn{exp(1/\sqrt{\sum \delta_i})}
#'
#' The model is well calibrated if the estimated \eqn{\alpha} coefficient is equal to 1.
#'
#' @references
#' `r format_bib("vanhouwelingen_2000")`
#'
#' @family calibration survival measures
#' @family distr survival measures
#' @export
MeasureSurvCalibrationAlpha = R6Class("MeasureSurvCalibrationAlpha",
  inherit = MeasureSurv,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(se = FALSE) {
      super$initialize(
        id = ifelse(se, "surv.calib_alpha_se", "surv.calib_alpha"),
        range = c(-Inf, Inf),
        minimize = FALSE,
        predict_type = "distr",
        man = "mlr3proba::mlr_measures_surv.calib_alpha",
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
      deaths = sum(prediction$truth[, 2])

      if (self$se) {
        return(exp(1/sqrt(deaths)))
      } else {
        haz = prediction$distr$cumHazard(data = matrix(prediction$truth[,1], nrow = 1))
        # cumulative hazard should only be infinite if only censoring occurs at the final time-point
        haz[haz == Inf] = 0
        return(deaths / sum(haz))
      }
    }
  )
)
