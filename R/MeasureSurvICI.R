#' @template surv_measure
#' @templateVar title Integrated Calibration Index
#' @templateVar fullname MeasureSurvICI
#'
#' @description
#' Calculates the Integrated Calibration Index (ICI), see Austin et al. (2020).
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
#' Note that we substitute any \eqn{\hat{P}_{t_0} = 1} with \eqn{0.9999} and any
#' \eqn{\hat{P}_{t_0} = 0} with \eqn{0.0001} to avoid arithmetic issues arising
#' from calculating \eqn{log(0)}.
#' From this model, the *smoothed* probability of occurrence at \eqn{t_0} for
#' observation \eqn{i} is obtained as \eqn{\hat{P}_i^c(t_0)}.
#'
#' The **Integrated Calibration Index** is then computed as:
#' \deqn{ICI = \frac{1}{N} \sum_{i=1}^N | \hat{P}_i^c(t_0) - \hat{P}_i(t_0) |}
#'
#' This measure evaluates **point-calibration** at a specific time point, which
#' must be specified by the user.
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
#'
#' @references
#' `r format_bib("austin2020")`
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
#' # ICI at median test time
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

      # time point for calibration
      time = self$param_set$values$time %??% median(times)

      # get cdf at the specified time point
      extend_times_cdf = getFromNamespace("C_Vec_WeightedDiscreteCdf", ns = "distr6")
      pred_times = as.numeric(colnames(surv))
      cdf = as.vector(extend_times_cdf(time, pred_times, cdf = t(1 - surv), TRUE, FALSE))
      # to avoid log(0) later, same as in paper's Appendix
      cdf[cdf == 1] = 0.9999
      cdf[cdf == 0] = 0.0001

      # get the cdf complement (survival) log-log transformed
      cll = log(-log(1 - cdf))

      hare_fit = polspline::hare(data = times, delta = status, cov = as.matrix(cll))
      smoothed_cdf = polspline::phare(q = time, cov = cll, fit = hare_fit)

      method = self$param_set$values$method
      if (method == "ICI") {
        # Mean difference (ICI)
        result = mean(abs(cdf - smoothed_cdf))
      } else if (method == "E50") {
        # Median (E50)
        result = stats::median(abs(cdf - smoothed_cdf))
      } else if (method == "E90") {
        # 90th percentile (E90)
        result = stats::quantile(abs(cdf - smoothed_cdf), probs = 0.90)
      } else if (method == "Emax") {
        # Maximum absolute difference (Emax)
        result = max(abs(cdf - smoothed_cdf))
      }

      return(result)
    }
  )
)

register_measure("surv.calib_index", MeasureSurvICI)
