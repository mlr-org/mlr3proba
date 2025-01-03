#' @template surv_measure
#' @templateVar title Integrated Calibration Index
#' @templateVar fullname MeasureSurvICI
#' @templateVar eps 1e-4
#' @template param_eps
#'
#' @description
#' Calculates the Integrated Calibration Index (ICI), which is the absolute
#' difference between predicted survival probabilities and smoothed survival
#' frequencies (calculated using hazard regression via the \CRANpkg{polspline})
#' at a specific time point.
#'
#' @references
#' `r format_bib("austin2020")`
#'
#' @family calibration survival measures
#' @family distr survival measures
#' @export
MeasureSurvICI = R6Class("MeasureSurvICI",
  inherit = MeasureSurv,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        time = p_dbl(0, Inf),
        method = p_fct(default = "ICI", levels = c("ICI", "E50", "E90", "Emax"))
      )
      param_set$set_values(method = "ICI")

      super$initialize(
        id = "surv.calib_index",
        packages = c("polspline"),
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr",
        label = "Integrated Calibration Index",
        man = "mlr3proba::mlr_measures_surv.calib_index",
        param_set = param_set
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      # test set survival outcome
      times = prediction$truth[, 1L]
      status = prediction$truth[, 2L]

      # get predicted survival matrix
      if (inherits(prediction$data$distr, "array")) {
        surv = prediction$data$distr
        if (length(dim(surv)) == 3L) {
          # survival 3d array, extract median
          surv = .ext_surv_mat(arr = surv, which.curve = 0.5)
        }
      } else {
        stop("Distribution prediction does not have a survival matrix or array
             in the $data$distr slot")
      }

      # time point for calibration
      time = self$param_set$values$time %??% median(times)

      # get cdf at the specified time point
      extend_times_cdf = getFromNamespace("C_Vec_WeightedDiscreteCdf", ns = "distr6")
      pred_times = as.numeric(colnames(surv))
      cdf = as.vector(extend_times_cdf(time, pred_times, cdf = t(1 - surv), TRUE, FALSE))
      #browser()
      # cdf 1 => 0.9999

      # get the cdf complement (survival) log-log transformed
      llsurv = log(-log(1 - cdf))

      hare_fit = polspline::hare(data = times, delta = status, cov = as.matrix(llsurv))
      cdf_hare = polspline::phare(q = time, cov = llsurv, fit = hare_fit)

      method = self$param_set$values$method
      if (method == "ICI") {
        # Mean difference (ICI)
        result = mean(abs(cdf - cdf_hare))
      } else if (method == "E50") {
        # Median (E50)
        result = median(abs(cdf - cdf_hare))
      } else if (method == "E90") {
        # 90th percentile (E90)
        result = quantile(abs(cdf - cdf_hare), probs = 0.90)
      } else if (method == "Emax") {
        # Maximum absolute difference (Emax)
        result = max(abs(cdf - cdf_hare))
      }

      return(result)
    }
  )
)

register_measure("surv.calib_index", MeasureSurvICI)
