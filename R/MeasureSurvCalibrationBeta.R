#' @template surv_measure
#' @templateVar title Van Houwelingen's Calibration Beta
#' @templateVar fullname MeasureSurvCalibrationBeta
#'
#' @description
#' This calibration method fits the predicted linear predictor from a Cox PH
#' model as the only predictor in a new Cox PH model with the test data as
#' the response.
#' \deqn{h(t|x) = h_0(t)exp(\beta \times lp)}
#' where \eqn{lp} is the predicted linear predictor on the test data.
#'
#' The model is well calibrated if the estimated \eqn{\hat{\beta}} coefficient
#' (returned score) is equal to 1.
#'
#' **Note**: Assumes fitted model is Cox PH (i.e. has an `lp` prediction type).
#'
#' @section Parameter details:
#' - `se` (`logical(1)`)\cr
#' If `TRUE` then return standard error of the measure which is the standard
#' error of the estimated coefficient \eqn{se_{\hat{\beta}}} from the Cox PH model.
#' If `FALSE` (default) then returns the estimated coefficient \eqn{\hat{\beta}}.
#' - `method` (`character(1)`)\cr
#' Returns \eqn{\hat{\beta}} if equal to `ratio` (default) and \eqn{|1-\hat{\beta}|}
#' if `diff`.
#' With `diff`, the output score can be minimized and for example be used for
#' tuning purposes.
#' This parameter takes effect only if `se` is `FALSE`.
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
        id = "surv.calib_beta",
        range = range,
        minimize = minimize,
        predict_type = "lp",
        label = "Van Houwelingen's Beta",
        man = "mlr3proba::mlr_measures_surv.calib_beta",
        param_set = ps
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      df = data.frame(truth = prediction$truth, lp = prediction$lp)
      fit = try(summary(survival::coxph(truth ~ lp, data = df)), silent = TRUE)

      if (class(fit)[1] == "try-error") {
        return(NA)
      } else {
        ps = self$param_set$values

        if (ps$se) {
          return(fit$coefficients[,"se(coef)"])
        } else {
          out = fit$coefficients[,"coef"]

          if (ps$method == "diff") {
            out = abs(1 - out)
          }

          return(out)
        }
      }
    }
  )
)

register_measure("surv.calib_beta", MeasureSurvCalibrationBeta)
