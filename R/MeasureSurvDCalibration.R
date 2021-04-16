#' @template surv_measure
#' @templateVar title D-Calibration
#' @templateVar fullname MeasureSurvDCalibration
#'
#' @description
#' This calibration method is defined by calculating
#' \deqn{s = B/n \sum_i (P_i - n/B)^2}
#' where \eqn{B} is number of 'buckets', \eqn{n} is the number of predictions,
#' and \eqn{P_i} is the predicted number of deaths in the \eqn{i}th interval
#' [0, 100/B), [100/B, 50/B),....,[(B - 100)/B, 1).
#'
#' A model is well-calibrated if `s ~ Unif(B)`, tested with `chisq.test`
#'  (`p > 0.05` if well-calibrated).
#' Model `i` is better calibrated than model `j` if `s_i < s_j`.
#'
#' @details
#' This measure can either return the test statistic or the p-value from the `chisq.test`.
#' The former is useful for model comparison whereas the latter is useful for determining if a model
#' is well-calibration. If `chisq = FALSE` and `m` is the predicted value then you can manually
#' compute the p.value with `pchisq(m, B - 1, lower.tail = FALSE)`.
#'
#' NOTE: This measure is still experimental both theoretically and in implementation. Results
#' should therefore only be taken as an indicator of performance and not for
#' conclusive judgements about model calibration.
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
    #' @param B (`integer(1)`) \cr
    #' Number of buckets to test for uniform predictions over. Default of `10` is recommended by
    #' Haider et al. (2020).
    #' @param chisq (`logical(1)`) \cr
    #' If `TRUE` returns the p.value of the corresponding chisq.test instead of the measure.
    #' Otherwise this can be performed manually with `pchisq(m, B - 1, lower.tail = FALSE)`.
    #' `p > 0.05` indicates well-calibrated.
    initialize = function(B = 10L, chisq = FALSE) {
      super$initialize(
        id = "surv.dcalib",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr",
        man = "mlr3proba::mlr_measures_surv.dcalib",
      )

      private$.B = assert_integerish(B)
      private$.chisq = assert_flag(chisq)
    }
  ),

  active = list(
    #' @field B (`integer(1)`) \cr
    #' Number of buckets to test for uniform predictions over. Default of `10` is recommended by
    #' Haider et al. (2020).
    B = function(x) {
      if (!missing(x)) {
        private$.B = assert_integerish(x)
      } else {
        return(private$.B)
      }
    },

    #' @field chisq `(logical(1))` \cr
    #' If `TRUE` returns the p.value of the corresponding chisq.test instead of the measure.
    #' Otherwise this can be performed manually with `pchisq(m, B - 1, lower.tail = FALSE)`.
    #' `p > 0.05` indicates well-calibrated.
    chisq = function(x) {
      if (!missing(x)) {
        private$.chisq = assert_flag(x)
      } else {
        return(private$.chisq)
      }
    }
  ),

  private = list(
    .B = 10L,
    .chisq = FALSE,
    .score = function(prediction, ...) {

      # initialize buckets
      bj = numeric(self$B)
      # predict individual probability of death at observed event time
      si = as.numeric(prediction$distr$survival(data = matrix(prediction$truth[, 1L], nrow = 1L)))
      # remove zeros
      si = map_dbl(si, function(.x) max(.x, 1e-5))
      # index of associated bucket
      js = ceiling(self$B * si)

      # could remove loop for dead observations but needed for censored ones and minimal overhead
      # in combining both
      for (i in seq_along(si)) {
        ji = js[[i]]
        if (prediction$truth[i, 2L] == 1L) {
          # dead observations contribute 1 to their index
          bj[ji] = bj[ji] + 1
        } else {
          # uncensored observations spread across buckets with most weighting on penultimate
          for (k in seq.int(ji - 1)) {
            bj[k] = bj[k] + 1/(self$B * si[[i]])
          }
          bj[ji] = bj[ji] + (1 - (ji - 1)/(self$B * si[[i]]))
        }
      }

      if (self$chisq) {
        return(stats::chisq.test(bj)$p.value)
      } else {
        return((self$B/length(si)) * sum((bj - length(si)/self$B)^2))
      }
    }
  )
)
