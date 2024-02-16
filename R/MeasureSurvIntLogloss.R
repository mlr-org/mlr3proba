#' @template surv_measure
#' @templateVar title Integrated Log-Likelihood
#' @templateVar fullname MeasureSurvIntLogloss
#' @template param_integrated
#' @template param_times
#' @template param_tmax
#' @template param_pmax
#' @template param_method
#' @template param_se
#' @template param_proper
#' @templateVar eps 1e-3
#' @template param_eps
#' @template param_erv
#'
#' @description
#' Calculates the **Integrated Survival Log-Likelihood** (ISLL) or Integrated
#' Logarithmic (log) Loss, aka integrated cross entropy.
#'
#' @details
#' For an individual who dies at time \eqn{t}, with predicted Survival function, \eqn{S}, the
#' probabilistic log loss at time \eqn{t^*}{t*} is given by
#' \deqn{L_{ISLL}(S,t|t^*) = - [log(1 - S(t^*))I(t \le t^*, \delta = 1)(1/G(t))] - [log(S(t^*))I(t > t^*)(1/G(t^*))]}
#' where \eqn{G} is the Kaplan-Meier estimate of the censoring distribution.
#'
#' The re-weighted ISLL, RISLL is given by
#' \deqn{L_{RISLL}(S,t|t^*) = - [log(1 - S(t^*))I(t \le t^*, \delta = 1)(1/G(t))] - [log(S(t^*))I(t > t^*)(1/G(t))]}
#' where \eqn{G} is the Kaplan-Meier estimate of the censoring distribution, i.e. always
#' weighted by \eqn{G(t)}.
#' RISLL is strictly proper when the censoring distribution is independent
#' of the survival distribution and when G is fit on a sufficiently large dataset.
#' ISLL is never proper.
#' Use `proper = FALSE` for ISLL and `proper = TRUE` for RISLL.
#' Results may be very different if many observations are censored at the last
#' observed time due to division by 1/`eps` in `proper = TRUE`.
#'
#' @template details_trainG
#'
#' @references
#' `r format_bib("graf_1999")`
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvIntLogloss = R6::R6Class("MeasureSurvIntLogloss",
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
        integrated = p_lgl(default = TRUE),
        times = p_uty(),
        t_max = p_dbl(0),
        p_max = p_dbl(0, 1),
        method = p_int(1L, 2L, default = 2L),
        se = p_lgl(default = FALSE),
        proper = p_lgl(default = FALSE),
        eps = p_dbl(0, 1, default = 1e-3),
        ERV = p_lgl(default = FALSE)
      )
      ps$values = list(
        integrated = TRUE, method = 2L, se = FALSE,
        proper = FALSE, eps = 1e-3, ERV = ERV
      )

      range = if (ERV) c(-Inf, 1) else c(0, Inf)

      super$initialize(
        param_set = ps,
        id = "surv.intlogloss",
        range = range,
        minimize = !ERV,
        packages = "distr6",
        predict_type = "distr",
        label = "Integrated Log Loss",
        man = "mlr3proba::mlr_measures_surv.intlogloss"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...) {
      ps = self$param_set$values

      if (ps$ERV) return(.scoring_rule_erv(self, prediction, task, train_set))
      nok = sum(!is.null(ps$times), !is.null(ps$t_max), !is.null(ps$p_max)) > 1
      if (nok) {
        stop("Only one of `times`, `t_max`, and `p_max` should be provided")
      }
      if (!ps$integrated) {
        msg = "If `integrated=FALSE` then `times` should be a scalar numeric."
        assert_numeric(ps$times, len = 1, .var.name = msg)
      } else {
        if (!is.null(ps$times) && length(ps$times) == 1) {
          ps$integrated = FALSE
        }
      }

      x = as.integer(!is.null(task)) + as.integer(!is.null(train_set))
      if (x == 1) {
        stop("Either 'task' and 'train_set' should be passed to measure or neither.")
      } else if (x) {
        train = task$truth(train_set)
      } else {
        train = NULL
      }

      score = weighted_survival_score("intslogloss", truth = prediction$truth,
        distribution = prediction$data$distr, times = ps$times, t_max = ps$t_max,
        p_max = ps$p_max, proper = ps$proper, train = train, eps = ps$eps)

      if (ps$se) {
        integrated_se(score, ps$integrated)
      } else {
        integrated_score(score, ps$integrated, ps$method)
      }
    }
  )
)

register_measure("surv.intlogloss", MeasureSurvIntLogloss)
