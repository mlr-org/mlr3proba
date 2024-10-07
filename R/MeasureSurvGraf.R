#' @template surv_measure
#' @templateVar title Integrated Brier Score
#' @templateVar fullname MeasureSurvGraf
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
#' @aliases MeasureSurvBrier mlr_measures_surv.brier
#'
#' @description
#' Calculates the **Integrated Survival Brier Score** (ISBS), Integrated Graf Score
#' or squared survival loss.
#'
#' @details
#' This measure has two dimensions: (test set) observations and time points.
#' For a specific individual \eqn{i}, with observed survival outcome \eqn{(t_i, \delta_i)}
#' (time and censoring indicator) and predicted survival function \eqn{S_i(t)}, the
#' *observation-wise* loss integrated across the time dimension up to the
#' time cutoff \eqn{\tau^*}, is:
#'
#' \deqn{L_{ISBS}(S_i, t_i, \delta_i) = \text{I}(t_i \leq \tau^*) \int^{\tau^*}_0  \frac{S_i^2(\tau) \text{I}(t_i \leq \tau, \delta=1)}{G(t_i)} + \frac{(1-S_i(\tau))^2 \text{I}(t_i > \tau)}{G(\tau)} \ d\tau}
#'
#' where \eqn{G} is the Kaplan-Meier estimate of the censoring distribution.
#'
#' The **re-weighted ISBS** (RISBS) is:
#'
#' \deqn{L_{RISBS}(S_i, t_i, \delta_i) = \delta_i \text{I}(t_i \leq \tau^*) \int^{\tau^*}_0  \frac{S_i^2(\tau) \text{I}(t_i \leq \tau) + (1-S_i(\tau))^2 \text{I}(t_i > \tau)}{G(t_i)} \ d\tau}
#'
#' which is always weighted by \eqn{G(t_i)} and removes the censored observations.
#'
#' RISBS is strictly proper when the censoring distribution is independent
#' of the survival distribution and when \eqn{G(t)} is fit on a sufficiently large dataset.
#' ISBS is never proper. Use `proper = FALSE` for ISBS and `proper = TRUE` for RISBS.
#' Results may be very different if many observations are
#' censored at the last observed time due to division by \eqn{1/eps} in `proper = TRUE`.
#'
#' @template details_method
#' @template details_trainG
#' @template details_tmax
#'
#' @references
#' `r format_bib("graf_1999")`
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvGraf = R6::R6Class("MeasureSurvGraf",
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
      ps$set_values(
        integrated = TRUE, method = 2L, se = FALSE,
        proper = FALSE, eps = 1e-3, ERV = ERV
      )

      range = if (ERV) c(-Inf, 1) else c(0, Inf)

      super$initialize(
        param_set = ps,
        id = "surv.graf",
        range = range,
        minimize = !ERV,
        packages = character(),
        predict_type = "distr",
        label = "Integrated Graf Score",
        man = "mlr3proba::mlr_measures_surv.graf",
      )
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...) {
      ps = self$param_set$values
      # times must be unique, sorted and positive numbers
      times = assert_numeric(ps$times, lower = 0, any.missing = FALSE,
                             unique = TRUE, sorted = TRUE, null.ok = TRUE)
      # ERV score
      if (ps$ERV) return(.scoring_rule_erv(self, prediction, task, train_set))

      nok = sum(!is.null(times), !is.null(ps$t_max), !is.null(ps$p_max)) > 1
      if (nok) {
        stop("Only one of `times`, `t_max`, and `p_max` should be provided")
      }

      if (!ps$integrated) {
        msg = "If `integrated=FALSE` then `times` should be a scalar numeric."
        assert_numeric(times, len = 1L, .var.name = msg)
      } else {
        if (!is.null(times) && length(times) == 1L) {
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

      # `score` is a matrix, IBS(i,j) => n_test_obs x times
      score = weighted_survival_score("graf",
        truth = prediction$truth,
        distribution = prediction$data$distr, times = times,
        t_max = ps$t_max, p_max = ps$p_max, proper = ps$proper, train = train,
        eps = ps$eps
      )

      if (ps$se) {
        integrated_se(score, ps$integrated)
      } else {
        integrated_score(score, ps$integrated, ps$method)
      }
    }
  )
)

register_measure("surv.graf", MeasureSurvGraf)
register_measure("surv.brier", MeasureSurvGraf)
