#' @template surv_measure
#' @templateVar title Van Houwelingen's Calibration Alpha
#' @templateVar fullname MeasureSurvCalibrationAlpha
#'
#' @description
#' This calibration method is defined by estimating
#' \deqn{\hat{\alpha} = \sum \delta_i / \sum H_i(T_i)}
#' where \eqn{\delta} is the observed censoring indicator from the test data,
#' \eqn{H_i} is the predicted cumulative hazard, and \eqn{T_i} is the observed
#' survival time (event or censoring).
#'
#' The standard error is given by
#' \deqn{\hat{\alpha_{se}} = exp(1/\sqrt{\sum \delta_i})}
#'
#' The model is well calibrated if the estimated \eqn{\hat{\alpha}} coefficient
#' (returned score) is equal to 1.
#'
#' @section Parameter details:
#' - `se` (`logical(1)`)\cr
#' If `TRUE` then return standard error of the measure, otherwise the score
#' itself (default).
#' - `method` (`character(1)`)\cr
#' Returns \eqn{\hat{\alpha}} if equal to `ratio` (default) and \eqn{|1-\hat{\alpha}|} if equal to `diff`.
#' With `diff`, the output score can be minimized and for example be used for tuning purposes.
#' This parameter takes effect only if `se` is `FALSE`.
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
    #' @param method defines which output score to return, see "Parameter
    #' details" section.
    initialize = function(method = "ratio") {
      assert_choice(method, choices = c("ratio", "diff"))

      ps = ps(
        se = p_lgl(default = FALSE),
        method = p_fct(c("ratio", "diff"), default = "ratio")
      )
      ps$values = list(se = FALSE, method = method)
      range = if (method == "ratio") c(-Inf, Inf) else c(0, Inf)
      minimize = ifelse(method == "ratio", FALSE, TRUE)

      super$initialize(
        id = "surv.calib_alpha",
        range = range,
        minimize = minimize,
        predict_type = "distr",
        label = "Van Houwelingen's Alpha",
        man = "mlr3proba::mlr_measures_surv.calib_alpha",
        param_set = ps
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      deaths = sum(prediction$truth[, 2])

      ps = self$param_set$values
      if (ps$se) {
        return(exp(1 / sqrt(deaths)))
      } else {
        if (inherits(prediction$distr, "VectorDistribution")) {
          haz = as.numeric(prediction$distr$cumHazard(
            data = matrix(prediction$truth[, 1], nrow = 1)
          ))
        } else {
          haz = diag(prediction$distr$cumHazard(prediction$truth[, 1]))
        }
        # cumulative hazard should only be infinite if only censoring occurs at the final time-point
        haz[haz == Inf] = 0
        out = deaths / sum(haz)

        if (ps$method == "diff") {
          out = abs(1 - out)
        }

        return(out)
      }
    }
  )
)

register_measure("surv.calib_alpha", MeasureSurvCalibrationAlpha)
