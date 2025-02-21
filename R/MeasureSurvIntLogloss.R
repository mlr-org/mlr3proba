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
#' @template param_remove_obs
#'
#' @description
#' Calculates the **Integrated Survival Log-Likelihood** (ISLL) or Integrated
#' Logarithmic (log) Loss, aka integrated cross entropy.
#'
#' @details
#' This measure has two dimensions: (test set) observations and time points.
#' For a specific individual \eqn{i} from the test set, with observed survival
#' outcome \eqn{(t_i, \delta_i)} (time and censoring indicator) and predicted
#' survival function \eqn{S_i(t)}, the *observation-wise* loss integrated across
#' the time dimension up to the time cutoff \eqn{\tau^*}, is:
#'
#' \deqn{L_{ISLL}(S_i, t_i, \delta_i) = - \int^{\tau^*}_0  \frac{log[1-S_i(\tau)] \text{I}(t_i \leq \tau, \delta_i=1)}{G(t_i)} + \frac{\log[S_i(\tau)] \text{I}(t_i > \tau)}{G(\tau)} \ d\tau}
#'
#' where \eqn{G} is the Kaplan-Meier estimate of the censoring distribution.
#'
#' The **re-weighted ISLL** (RISLL) is:
#'
#' \deqn{L_{RISLL}(S_i, t_i, \delta_i) = -\delta_i \frac{\int^{\tau^*}_0  \log[1-S_i(\tau)]) \text{I}(t_i \leq \tau) + \log[S_i(\tau)] \text{I}(t_i > \tau) \ d\tau}{G(t_i)}}
#'
#' which is always weighted by \eqn{G(t_i)} and is equal to zero for a censored subject.
#'
#' To get a single score across all \eqn{N} observations of the test set, we
#' return the average of the time-integrated observation-wise scores:
#' \deqn{\sum_{i=1}^N L(S_i, t_i, \delta_i) / N}
#'
#' @template properness
#' @templateVar improper_id ISLL
#' @templateVar proper_id RISLL
#' @template which_times
#' @template details_method
#' @template details_trainG
#' @template details_tmax
#'
#' @references
#' `r format_bib("graf_1999", "sonabend_2024", "kvamme_2023")`
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @template example_scoring_rules
#' @export
MeasureSurvIntLogloss = R6Class("MeasureSurvIntLogloss",
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
        ERV = p_lgl(default = FALSE),
        remove_obs = p_lgl(default = FALSE)
      )
      ps$set_values(
        integrated = TRUE, method = 2L, se = FALSE,
        proper = FALSE, eps = 1e-3, ERV = ERV, remove_obs = FALSE
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
      score = weighted_survival_score("intslogloss",
        truth = prediction$truth,
        distribution = prediction$data$distr, times = times,
        t_max = ps$t_max, p_max = ps$p_max, proper = ps$proper, train = train,
        eps = ps$eps, remove_obs = ps$remove_obs
      )

      if (ps$se) {
        integrated_se(score, ps$integrated)
      } else {
        integrated_score(score, ps$integrated, ps$method)
      }
    }
  )
)

register_measure("surv.intlogloss", MeasureSurvIntLogloss)
