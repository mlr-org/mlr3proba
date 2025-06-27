#' @template surv_measure
#' @templateVar title Negative Log-Likelihood
#' @templateVar fullname MeasureSurvLogloss
#' @templateVar eps 1e-6
#' @template param_eps
#' @template param_erv
#'
#' @description
#' Calculates the cross-entropy, or negative log-likelihood (NLL) or logarithmic (log) loss.
#'
#' @details
#' The (observation-wise) Log-Likelihood is defined as the negative logarithm of
#' the predicted probability density function \eqn{f_i}, evaluated at the
#' observation time \eqn{t_i} (event or censoring):
#'
#' \deqn{L_{NLL}(S_i,t_i) = -\log{[f_i(t_i)]}}
#'
#' This loss does not take into account the censoring status of an observation,
#' treating all outcomes as events, see Sonabend et al. (2024).
#'
#' To get a single score across all \eqn{N} observations of the test set, we
#' return the average of the observation-wise scores:
#'
#' \deqn{\sum_{i=1}^N L_{NLL}(S_i, t_i) / N}
#'
#' @template details_interp
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
        eps = p_dbl(0, 1, default = 1e-6),
        ERV = p_lgl(default = FALSE)
      )
      ps$set_values(eps = 1e-6, ERV = ERV)

      range = if (ERV) c(-Inf, 1) else c(0, Inf)

      super$initialize(
        id = "surv.logloss",
        range = range,
        minimize = !ERV,
        predict_type = "distr",
        label = "Negative Log-Likelihood",
        man = "mlr3proba::mlr_measures_surv.logloss",
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

      # get survival matrix
      surv_mat = .get_surv_matrix(prediction)
      pred_times = as.numeric(colnames(surv_mat))

      res = vapply(seq_len(n_obs), function(obs_index) {
        # event time or censoring time
        outcome_time = test_times[obs_index]

        # predicted survival curve for observation
        surv_pred = list(surv = surv_mat[obs_index, ], time = pred_times)

        # predicted pdf at observed time
        .interp_pdf(surv_pred, outcome_time)
      }, numeric(1))

      mean(-log(pmax(pv$eps, res)))
    }
  )
)

register_measure("surv.logloss", MeasureSurvLogloss)
