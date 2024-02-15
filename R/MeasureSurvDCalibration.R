#' @template surv_measure
#' @templateVar title D-Calibration
#' @templateVar fullname MeasureSurvDCalibration
#'
#' @description
#' This calibration method is defined by calculating the following statistic:
#' \deqn{s = B/n \sum_i (P_i - n/B)^2}
#' where \eqn{B} is number of 'buckets' (that equally divide \eqn{[0,1]} into intervals),
#' \eqn{n} is the number of predictions, and \eqn{P_i} is the observed proportion
#' of observations in the \eqn{i}th interval. An observation is assigned to the
#' \eqn{i}th bucket, if its predicted survival probability at the time of event
#' falls within the corresponding interval.
#' This statistic assumes that censoring time is independent of death time.
#'
#' A model is well-calibrated if \eqn{s \sim Unif(B)}, tested with `chisq.test`
#'  (\eqn{p > 0.05} if well-calibrated).
#' Model \eqn{i} is better calibrated than model \eqn{j} if \eqn{s(i) < s(j)},
#' meaning that *lower values* of this measure are preferred.
#'
#' @details
#' This measure can either return the test statistic or the p-value from the `chisq.test`.
#' The former is useful for model comparison whereas the latter is useful for determining if a model
#' is well-calibrated. If `chisq = FALSE` and `s` is the predicted value then you can manually
#' compute the p.value with `pchisq(s, B - 1, lower.tail = FALSE)`.
#'
#' NOTE: This measure is still experimental both theoretically and in implementation. Results
#' should therefore only be taken as an indicator of performance and not for
#' conclusive judgements about model calibration.
#'
#' @section Parameter details:
#' - `B` (`integer(1)`) \cr
#' Number of buckets to test for uniform predictions over.
#' Default of `10` is recommended by Haider et al. (2020).
#' Changing this parameter affects `truncate`.
#' - `chisq` (`logical(1)`) \cr
#' If `TRUE` returns the p-value of the corresponding chisq.test instead of the measure.
#' Default is `FALSE` and returns the statistic `s`.
#' You can manually get the p-value by executing `pchisq(s, B - 1, lower.tail = FALSE)`.
#' The null hypothesis is that the model is D-calibrated.
#' - `truncate` (`double(1)`) \cr
#' This parameter controls the upper bound of the output statistic,
#' when `chisq` is `FALSE`. We use `truncate = Inf` by default but \eqn{10} may be sufficient
#' for most purposes, which corresponds to a p-value of 0.35 for the chisq.test using
#' \eqn{B = 10} buckets. Values \eqn{>10} translate to even lower p-values and thus
#' less calibrated models. If the number of buckets \eqn{B} changes, you probably will want to
#' change the `truncate` value as well to correspond to the same p-value significance.
#' Note that truncation may severely limit automated tuning with this measure.
#'
#' @references
#' `r format_bib("haider_2020")`
#'
#' @family calibration survival measures
#' @family distr survival measures
#' @export
MeasureSurvDCalibration = R6Class("MeasureSurvDCalibration",
  inherit = MeasureSurv,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        B = p_int(1, default = 10),
        chisq = p_lgl(default = FALSE),
        truncate = p_dbl(lower = 0, upper = Inf, default = Inf)
      )
      ps$values = list(B = 10L, chisq = FALSE, truncate = Inf)

      super$initialize(
        id = "surv.dcalib",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr",
        label = "D-Calibration",
        man = "mlr3proba::mlr_measures_surv.dcalib",
        param_set = ps
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      ps = self$param_set$values
      B = ps$B

      # initialize buckets
      bj = numeric(B)
      true_times = prediction$truth[, 1L]

      # predict individual probability of death at observed event time
      # bypass distr6 construction if possible
      if (inherits(prediction$data$distr, "array")) {
        surv = prediction$data$distr
        if (length(dim(surv)) == 3) {
          # survival 3d array, extract median
          surv = .ext_surv_mat(arr = surv, which.curve = 0.5)
        }
        times = as.numeric(colnames(surv))

        extend_times = getFromNamespace("C_Vec_WeightedDiscreteCdf", ns = "distr6")
        si = diag(extend_times(true_times, times, cdf = t(1 - surv), FALSE, FALSE))
      } else {
        distr = prediction$distr
        if (inherits(distr, c("Matdist", "Arrdist"))) {
          si = diag(distr$survival(true_times))
        } else { # VectorDistribution or single Distribution, e.g. WeightDisc()
          si = as.numeric(distr$survival(data = matrix(true_times, nrow = 1L)))
        }
      }
      # remove zeros
      si = map_dbl(si, function(.x) max(.x, 1e-5))
      # index of associated bucket
      js = ceiling(B * si)

      # could remove loop for dead observations but needed for censored ones and minimal overhead
      # in combining both
      for (i in seq_along(si)) {
        ji = js[[i]]
        if (prediction$truth[i, 2L] == 1L) {
          # dead observations contribute 1 to their index
          bj[ji] = bj[ji] + 1
        } else {
          # censored observations spread across buckets with most weighting on penultimate
          for (k in seq.int(ji - 1)) {
            bj[k] = bj[k] + 1 / (B * si[[i]])
          }
          bj[ji] = bj[ji] + (1 - (ji - 1) / (B * si[[i]]))
        }
      }

      if (ps$chisq) {
        return(stats::chisq.test(bj)$p.value)
      } else {
        return(min(ps$truncate, (B / length(si)) * sum((bj - length(si) / B)^2)))
      }
    }
  )
)

register_measure("surv.dcalib", MeasureSurvDCalibration)
