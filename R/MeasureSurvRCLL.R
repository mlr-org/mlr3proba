#' @template surv_measure
#' @templateVar title Right-Censored Log Loss
#' @templateVar fullname MeasureSurvRCLL
#' @templateVar eps 1e-6
#' @template param_eps
#' @template param_erv
#'
#' @description
#' Calculates the right-censored log-likelihood (RCLL) or logarithmic loss,
#' introduced by Avati et al. (2020).
#'
#' @details
#' The observation-wise RCLL is defined by:
#'
#' \deqn{L_{RCLL}(S_i, t_i, \delta_i) = -log[\delta_i f_i(t_i) + (1 - \delta_i) S_i(t_i)]}
#'
#' where \eqn{\delta_i} is the censoring indicator, \eqn{f_i} the predicted probability
#' density function and \eqn{S_i} the predicted survival function for observation \eqn{i}.
#' RCLL is proper given that censoring and survival distribution are independent, see Rindt et al. (2022).
#' Simulation studies by Sonabend et al. (2024) provide strong empirical evidence
#' supporting the properness of this score.
#' See section **Interpolation** for implementation details.
#'
#' To get a single score across all \eqn{N} observations of the test set, we
#' return the average of the observation-wise scores:
#'
#' \deqn{\sum_{i=1}^N L_{RCLL}(S_i, t_i, \delta_i) / N}
#'
#' @template details_interp
#'
#' @references
#' `r format_bib("avati_2020", "rindt_2022", "sonabend_2024")`
#'
#' @family Probabilistic survival measures
#' @family distr survival measures
#' @export
MeasureSurvRCLL = R6Class("MeasureSurvRCLL",
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
        eps = p_dbl(0, 1, default = 1e-6),
        ERV = p_lgl(default = FALSE)
      )
      ps$set_values(eps = 1e-6, ERV = ERV)

      range = if (ERV) c(-Inf, 1) else c(0, Inf)

      super$initialize(
        id = "surv.rcll",
        minimize = !ERV,
        predict_type = "distr",
        label = "Right-Censored Log-Likelihood",
        man = "mlr3proba::mlr_measures_surv.rcll",
        range = range,
        param_set = ps
      )

      invisible(self)
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...) {
      pv = self$param_set$values

      if (pv$ERV) {
        return(.scoring_rule_erv(self, prediction, task, train_set))
      }

      truth = prediction$truth
      n_obs = length(truth)
      test_times = truth[, 1L]
      test_status = truth[, 2L]

      # get survival matrix
      surv_mat = .get_surv_matrix(prediction)
      pred_times = as.numeric(colnames(surv_mat))

      res = vapply(seq_len(n_obs), function(obs_index) {
        # event time or censoring time
        outcome_time = test_times[obs_index]

        # predicted survival curve for observation
        surv_pred = list(surv = surv_mat[obs_index, ], time = pred_times)

        if (test_status[obs_index] == 1) {
          # event => use f(t)
          .interp_pdf(surv_pred, outcome_time)
        } else {
          # censored => use S(t)
          .interp_surv(surv_pred, outcome_time)
        }
      }, numeric(1))

      mean(-log(pmax(pv$eps, res)))
    }
  )
)

register_measure("surv.rcll", MeasureSurvRCLL)
