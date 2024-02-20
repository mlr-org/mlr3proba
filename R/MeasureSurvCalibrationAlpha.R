#' @template surv_measure
#' @templateVar title Van Houwelingen's Calibration Alpha
#' @templateVar fullname MeasureSurvCalibrationAlpha
#' @templateVar eps 1e-3
#' @template param_eps
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
#' Returns \eqn{\hat{\alpha}} if equal to `ratio` (default) and
#' \eqn{|1-\hat{\alpha}|} if equal to `diff`.
#' With `diff`, the output score can be minimized and for example be used for
#' tuning purposes. This parameter takes effect only if `se` is `FALSE`.
#' - `truncate` (`double(1)`) \cr
#' This parameter controls the upper bound of the output score.
#' We use `truncate = Inf` by default (so no truncation) and it's up to the user
#' **to set this up reasonably** given the chosen `method`.
#' Note that truncation may severely limit automated tuning with this measure
#' using `method = diff`.
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
        eps = p_dbl(0, 1, default = 1e-3),
        se = p_lgl(default = FALSE),
        method = p_fct(c("ratio", "diff"), default = "ratio"),
        truncate = p_dbl(lower = -Inf, upper = Inf, default = Inf)
      )
      ps$values = list(eps = 1e-3, se = FALSE, method = method, truncate = Inf)
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
      truth = prediction$truth
      all_times = truth[, 1] # both event times and censoring times
      status = truth[, 2]
      deaths = sum(status)

      ps = self$param_set$values
      if (ps$se) {
        return(exp(1 / sqrt(deaths)))
      } else {
        distr = prediction$data$distr

        # Bypass distr6 construction if underlying distr represented by array
        if (inherits(distr, "array")) {
          surv = distr
          if (length(dim(surv)) == 3) {
            # survival 3d array, extract median
            surv = .ext_surv_mat(arr = surv, which.curve = 0.5)
          }
          times = as.numeric(colnames(surv))

          extend_times_cdf = getFromNamespace("C_Vec_WeightedDiscreteCdf", ns = "distr6")
          # get survival probability for each test obs at observed time
          surv_all = diag(
            extend_times_cdf(all_times, times, cdf = t(1 - surv), FALSE, FALSE)
          )

          # H(t) = -log(S(t))
          cumhaz = -log(surv_all)
        } else {
          if (inherits(distr, "VectorDistribution")) {
            cumhaz = as.numeric(
              distr$cumHazard(data = matrix(all_times, nrow = 1))
            )
          } else {
            cumhaz = diag(as.matrix(distr$cumHazard(all_times)))
          }
        }

        # Inf => case where censoring occurs at last time point
        # 0   => case where survival probabilities are all 1
        cumhaz[cumhaz == Inf | cumhaz == 0] = ps$eps
        out = deaths / sum(cumhaz)

        if (ps$method == "diff") {
          out = abs(1 - out)
        }

        return(min(ps$truncate, out))
      }
    }
  )
)

register_measure("surv.calib_alpha", MeasureSurvCalibrationAlpha)
