#' @template surv_measure
#' @templateVar title Integrated Schmid Score
#' @templateVar fullname MeasureSurvSchmid
#' @template param_integrated
#' @template param_times
#' @template param_tmax
#' @template param_pmax
#' @templateVar eps 1e-3
#' @template param_eps
#' @template param_erv
#'
#' @description
#' Calculates the **Integrated Schmid Score** (ISS), aka integrated absolute loss.
#'
#' @details
#' This measure has two dimensions: (test set) observations and time points.
#' For a specific individual \eqn{i} from the test set, with observed survival
#' outcome \eqn{(t_i, \delta_i)} (time and censoring indicator) and predicted
#' survival function \eqn{S_i(t)}, the *observation-wise* estimator of the loss,
#' integrated across the time dimension up to the time cutoff \eqn{\tau^*}, is:
#'
#' \deqn{L_{ISS}(S_i, t_i, \delta_i) = \int^{\tau^*}_0  \frac{S_i(\tau) \text{I}(t_i \leq \tau, \delta=1)}{G(t_i)} + \frac{(1-S_i(\tau)) \text{I}(t_i > \tau)}{G(\tau)} \ d\tau}
#'
#' where \eqn{G} is the Kaplan-Meier estimate of the censoring distribution.
#'
#' The implementation uses the trapezoidal rule to approximate the integral over
#' time and the integral is normalized by the range of available evaluation times
#' (\eqn{\tau_{\text{max}} - \tau_{\text{min}}}).
#'
#' To get a single score across all \eqn{N} observations of the test set, we
#' return the average of the time-integrated observation-wise scores:
#' \deqn{\sum_{i=1}^N L(S_i, t_i, \delta_i) / N}
#'
#' @template properness
#' @templateVar id ISS
#' @template which_times
#' @template details_trainG
#' @template details_tmax
#' @template implementation_diffs
#'
#' @references
#' `r format_bib("schemper_2000", "schmid_2011", "sonabend_2024", "kvamme_2023")`
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @template example_scoring_rules
#' @export
MeasureSurvSchmid = R6Class("MeasureSurvSchmid",
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
        eps = p_dbl(0, 1, default = 1e-3),
        ERV = p_lgl(default = FALSE)
      )
      ps$set_values(integrated = TRUE, eps = 1e-3, ERV = ERV)

      range = if (ERV) c(-Inf, 1) else c(0, Inf)

      super$initialize(
        param_set = ps,
        id = "surv.schmid",
        range = range,
        minimize = !ERV,
        packages = "distr6",
        predict_type = "distr",
        label = "Integrated Schmid Score",
        man = "mlr3proba::mlr_measures_surv.schmid",
      )
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...) {
      ps = self$param_set$values

      # ERV score
      if (ps$ERV) return(.scoring_rule_erv(self, prediction, task, train_set))
      nok = sum(!is.null(ps$times), !is.null(ps$t_max), !is.null(ps$p_max)) > 1
      if (nok) {
        stop("Only one of `times`, `t_max`, and `p_max` should be provided")
      }
      if (!ps$integrated) {
        msg = "If `integrated=FALSE` then `times` should be a scalar numeric."
        assert_numeric(ps$times, len = 1L, .var.name = msg)
      } else {
        if (!is.null(ps$times) && length(ps$times) == 1L) {
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

      # `score` is a matrix, ISS(i,j) => [test_obs x times]
      score = .weighted_survival_score(
        loss = "schmid",
        truth = prediction$truth,
        distribution = prediction$data$distr,
        times = ps$times, t_max = ps$t_max, p_max = ps$p_max,
        train = train, eps = ps$eps
      )

      .integrated_score(score, ps$integrated)
    }
  )
)

register_measure("surv.schmid", MeasureSurvSchmid)
