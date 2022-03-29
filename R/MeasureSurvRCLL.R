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
        se = p_lgl(default = FALSE)
      )
      ps$values = list(eps = 1e-15, se = FALSE)

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
    .score = function(prediction, ...) {
      out = numeric(length(prediction$row_ids))
      truth = prediction$truth
      event = truth[, 2] == 1

      # uncensored -> pdf at outcome time (survived *this* long)
      out[event] = diag(prediction$distr$pdf(truth[event, 1]))
      # censored -> survival at outcome time (survived *at least* this long)
      out[!event] = diag(prediction$distr$survival(truth[!event, 1]))
      # prevent infinite log errors
      out[out == 0] = self$param_set$values$eps

      out = -log(out)

      if (self$param_set$values$se) {
        sd(out) / sqrt(length(out))
      } else {
        mean(out)
      }
    }
  )
)
