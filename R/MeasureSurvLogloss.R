#' @template surv_measure
#' @templateVar title Negative Log-Likelihood
#' @templateVar fullname MeasureSurvLogloss
#' @templateVar eps 1e-15
#' @template param_eps
#' @template param_se
#' @template param_erv
#'
#' @description
#' Calculates the cross-entropy, or negative log-likelihood (NLL) or logarithmic (log), loss.
#' @section Parameter details:
#' - `IPCW` (`logical(1)`)\cr
#' If `TRUE` (default) then returns the \eqn{L_{RNLL}} score (which is proper), otherwise the \eqn{L_{NLL}} score (improper). See Sonabend et al. (2024) for more details.
#'
#' @details
#' The Log Loss, in the context of probabilistic predictions, is defined as the
#' negative log probability density function, \eqn{f}, evaluated at the
#' observation time (event or censoring), \eqn{t},
#' \deqn{L_{NLL}(f, t) = -\log[f(t)]}
#'
#' The standard error of the Log Loss, L, is approximated via,
#' \deqn{se(L) = sd(L)/\sqrt{N}}{se(L) = sd(L)/\sqrt N}
#' where \eqn{N} are the number of observations in the test set, and \eqn{sd} is the standard
#' deviation.
#'
#' The **Re-weighted Negative Log-Likelihood** (RNLL) or IPCW (Inverse Probability Censoring Weighted) Log Loss is defined by
#' \deqn{L_{RNLL}(f, t, \delta) = - \frac{\delta \log[f(t)]}{G(t)}}
#' where \eqn{\delta} is the censoring indicator and \eqn{G(t)} is the Kaplan-Meier estimator of the
#' censoring distribution.
#' So only observations that have experienced the event are taking into account
#' for RNLL (i.e. \eqn{\delta = 1}) and both \eqn{f(t), G(t)} are calculated only at the event times.
#' If only censored observations exist in the test set, `NaN` is returned.
#'
#' @template details_trainG
#'
#' @references
#' `r format_bib("sonabend_2024")`
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvLogloss = R6Class("MeasureSurvLogloss",
  inherit = MeasureSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param ERV (`logical(1)`)\cr
    #'   Standardize measure against a Kaplan-Meier baseline
    #'   (Explained Residual Variation)
    initialize = function(ERV = FALSE) {
      assert_logical(ERV)

      ps = ps(
        eps = p_dbl(0, 1, default = 1e-15),
        se = p_lgl(default = FALSE),
        IPCW = p_lgl(default = TRUE),
        ERV = p_lgl(default = FALSE)
      )
      ps$set_values(eps = 1e-15, se = FALSE, IPCW = TRUE, ERV = ERV)

      range = if (ERV) c(-Inf, 1) else c(0, Inf)

      super$initialize(
        id = "surv.logloss",
        range = range,
        minimize = !ERV,
        predict_type = "distr",
        packages = "distr6",
        label = "Log Loss",
        man = "mlr3proba::mlr_measures_surv.logloss",
        param_set = ps
      )

      invisible(self)
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...) {
      if (self$param_set$values$ERV) {
        return(.scoring_rule_erv(self, prediction, task, train_set))
      }
      x = as.integer(!is.null(task)) + as.integer(!is.null(train_set))
      if (x == 1) {
        stop("Either 'task' and 'train_set' should be passed to measure or neither.")
      } else if (x) {
        train = task$truth(train_set)
      } else {
        train = NULL
      }

      ps = self$param_set$values

      if (ps$se) {
        ll = surv_logloss(prediction$truth, prediction$data$distr, ps$eps, ps$IPCW, train) # nolint
        sd(ll) / sqrt(length(ll))
      } else {
        mean(surv_logloss(prediction$truth, prediction$data$distr, ps$eps, ps$IPCW, train)) # nolint
      }
    }
  )
)

register_measure("surv.logloss", MeasureSurvLogloss)
