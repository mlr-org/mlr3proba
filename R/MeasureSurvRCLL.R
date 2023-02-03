#' @template surv_measure
#' @templateVar title Right-Censored Log loss
#' @templateVar fullname MeasureSurvRCLL
#'
#' @description
#' Calculates the right-censored logarithmic (log), loss.
#'
#' The RCLL, in the context of probabilistic predictions, is defined by
#' \deqn{L(f, t, \Delta) = -log(\Delta f(t) + (1 - \Delta) S(t))}
#' where \eqn{\Delta} is the censoring indicator.
#'
#' @template param_id
#' @template param_eps
#'
#' @description
#' Parameters
#' * `eps` (numeric(1)) - Value to set zero-valued scores to prevent log(0) errors, default `1e-15`.
#' * `se` (logical(1)) - If `TRUE` then returns standard error of the loss otherwise returns mean across all individual scores.
#' * `ERV` (logical(1)) - If `TRUE` then the Explained Residual Variation method is applied, which means the score is standardised against a Kaplan-Meier baseline.
#' * `na.rm` (logical(1)) - If `TRUE` (default) then removes any NAs in individual score calculations.
#'
#' @references
#' Avati, A., Duan, T., Zhou, S., Jung, K., Shah, N. H., & Ng, A. (2018).
#' Countdown Regression: Sharp and Calibrated Survival Predictions.
#' http://arxiv.org/abs/1806.08324
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvRCLL = R6::R6Class("MeasureSurvRCLL",
  inherit = MeasureSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        eps = p_dbl(0, 1, default = 1e-15),
        se = p_lgl(default = FALSE),
        ERV = p_lgl(default = FALSE),
        na.rm = p_lgl(default = TRUE)
      )
      ps$values = list(eps = 1e-15, se = FALSE, ERV = FALSE, na.rm = TRUE)

      super$initialize(
        id = "surv.rcll",
        minimize = TRUE,
        predict_type = "distr",
        packages = "distr6",
        label = "RCLL",
        man = "mlr3proba::mlr_measures_surv.rcll",
        range = c(-Inf, Inf),
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
      out = numeric(length(prediction$row_ids))
      truth = prediction$truth
      event = truth[, 2] == 1
      event_times = truth[event, 1]
      cens_times = truth[!event, 1]

      if (length(event_times) == 0) { # all censored
        # survival at outcome time (survived *at least* this long)
        out[!event] = diag(as.matrix(prediction$distr$survival(cens_times)))
      } else if (length(cens_times) == 0) { # all uncensored
        # pdf at outcome time (survived *this* long)
        out[event] = diag(as.matrix(prediction$distr$pdf(event_times)))
      } else { # mix
        out[event] = diag(as.matrix(prediction$distr$pdf(event_times)))
        out[!event] = diag(as.matrix(prediction$distr$survival(cens_times)))
      }

      # prevent infinite log errors
      out[out == 0] = self$param_set$values$eps

      out = -log(out)

      if (self$param_set$values$se) {
        sd(out, na.rm = self$param_set$values$na.rm) / sqrt(length(out))
      } else {
        mean(out, na.rm = self$param_set$values$na.rm)
      }
    }
  )
)
