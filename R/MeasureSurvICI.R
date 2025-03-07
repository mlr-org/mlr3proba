#' @template surv_measure
#' @templateVar title Integrated Calibration Index
#' @templateVar fullname MeasureSurvICI
#' @templateVar eps 1e-4
#' @template param_eps
#'
#' @description
#' Calculates the Integrated Calibration Index (ICI), which evaluates
#' **point-calibration** (i.e. at a specific time point), see Austin et al. (2020).
#'
#' @details
#' Each individual \eqn{i} from the test set, has an observed survival outcome
#' \eqn{(t_i, \delta_i)} (time and censoring indicator) and predicted survival
#' function \eqn{S_i(t)}.
#' The predicted probability of an event occurring before a specific time point
#' \eqn{t_0}, is defined as \eqn{\hat{P_i}(t_0) = F_i(t_0) = 1 - S_i(t_0)}.
#'
#' Using hazard regression (via the \CRANpkg{polspline} R package), a *smoothed*
#' calibration curve is estimated by fitting the following model:
#' \deqn{log(h(t)) = g(log(− log(1 − \hat{P}_{t_0})), t)}
#'
#' Note that we substitute probabilities \eqn{\hat{P}_{t_0} = 0} with a small
#' \eqn{\epsilon} number to avoid arithmetic issues (\eqn{log(0)}). Same with
#' \eqn{\hat{P}_{t_0} = 1}, we use \eqn{1 - \epsilon}.
#' From this model, the *smoothed* probability of occurrence at \eqn{t_0} for
#' observation \eqn{i} is obtained as \eqn{\hat{P}_i^c(t_0)}.
#'
#' The **Integrated Calibration Index** is then computed across the \eqn{N}
#' test set observations as:
#' \deqn{ICI = \frac{1}{N} \sum_{i=1}^N | \hat{P}_i^c(t_0) - \hat{P}_i(t_0) |}
#'
#' Therefore, a perfect calibration (smoothed probabilities match predicted
#' probabilities for all observations) yields \eqn{ICI = 0}, while the worst
#' possible score is \eqn{ICI = 1}.
#'
#' @section Parameter details:
#' - `time` (`numeric(1)`)\cr
#' The specific time point \eqn{t_0} at which calibration is evaluated.
#' If `NULL`, the median observed time from the test set is used.
#' - `method` (`character(1)`)\cr
#' Specifies the summary statistic used to calculate the final calibration score.
#'   - `"ICI"` (default): Uses the mean of absolute differences \eqn{| \hat{P}_i^c(t_0) - \hat{P}_i(t_0) |} across all observations.
#'   - `"E50"`: Uses the median of absolute differences instead of the mean.
#'   - `"E90"`: Uses the 90th percentile of absolute differences, emphasizing higher deviations.
#'   - `"Emax"`: Uses the maximum absolute difference, capturing the largest discrepancy between predicted and smoothed probabilities.
#' - `na.rm` (`logical(1)`)\cr
#' If `TRUE` (default) then removes any NAs/NaNs in the smoothed probabilities
#' \eqn{\hat{P}_i^c(t_0)} that may arise. A warning is issued nonetheless in such
#' cases.
#'
#' @references
#' `r format_bib("austin_2020")`
#'
#' @family calibration survival measures
#' @family distr survival measures
#' @examples
#' library(mlr3)
#'
#' # Define a survival Task
#' task = tsk("lung")
#'
#' # Create train and test set
#' part = partition(task)
#'
#' # Train Cox learner on the train set
#' cox = lrn("surv.coxph")
#' cox$train(task, row_ids = part$train)
#'
#' # Make predictions for the test set
#' p = cox$predict(task, row_ids = part$test)
#'
#' # ICI at median test set time
#' p$score(msr("surv.calib_index"))
#'
#' # ICI at specific time point
#' p$score(msr("surv.calib_index", time = 365))
#'
#' # E50 at specific time point
#' p$score(msr("surv.calib_index", method = "E50", time = 365))
#'
#' @export
MeasureSurvICI = R6Class("MeasureSurvICI",
  inherit = MeasureSurv,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        time = p_dbl(0, Inf),
        eps = p_dbl(0, 1, default = 1e-4),
        method = p_fct(default = "ICI", levels = c("ICI", "E50", "E90", "Emax")),
        na.rm = p_lgl(default = TRUE)
      )
      param_set$set_values(method = "ICI", eps = 1e-4, na.rm = TRUE)

      super$initialize(
        id = "surv.calib_index",
        packages = c("polspline"),
        range = c(0, 1),
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
      times  = prediction$truth[, 1L]
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

      pv = self$param_set$values

      # time point for calibration
      time = pv$time %??% median(times)

      # get cdf at the specified time point
      extend_times_cdf = getFromNamespace("C_Vec_WeightedDiscreteCdf", ns = "distr6")
      pred_times = as.numeric(colnames(surv))
      cdf = as.vector(extend_times_cdf(time, pred_times, cdf = t(1 - surv), TRUE, FALSE))
      # to avoid log(0) later, same as in paper's Appendix
      eps = pv$eps
      cdf[cdf == 1] = 1 - eps
      cdf[cdf == 0] = eps

      # get the cdf complement (survival) log-log transformed
      cll = log(-log(1 - cdf))

      hare_fit = polspline::hare(data = times, delta = status, cov = as.matrix(cll))
      smoothed_cdf = polspline::phare(q = time, cov = cll, fit = hare_fit)
      if (anyNA(smoothed_cdf)) {
        warning("`polspline::phare` fit resulted in NaN smoothed probabilities")
      }

      method = pv$method
      na.rm = pv$na.rm
      if (method == "ICI") {
        # Mean difference (ICI)
        result = mean(abs(cdf - smoothed_cdf), na.rm = na.rm)
      } else if (method == "E50") {
        # Median (E50)
        result = median(abs(cdf - smoothed_cdf), na.rm = na.rm)
      } else if (method == "E90") {
        # 90th percentile (E90)
        result = quantile(abs(cdf - smoothed_cdf), probs = 0.9, na.rm = na.rm)
      } else if (method == "Emax") {
        # Maximum absolute difference (Emax)
        result = max(abs(cdf - smoothed_cdf), na.rm = na.rm)
      }

      result
    }
  )
)

register_measure("surv.calib_index", MeasureSurvICI)
