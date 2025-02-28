#' @title Aalen Johansen Competing Risks Learner
#' @templateVar fullname LearnerCompRisksAalenJohansen
#' @templateVar id cmprsk.aalen
#' @template cmprsk_learner
#'
#' @description
#'
#' This learner estimates the Cumulative Incidence Function (CIF) for competing
#' risks using the empirical Aalen-Johansen (AJ) estimator.
#' The probability of transitioning to each competing event is computed via the
#' [survfit][survival::survfit.formula()] function.
#'
#' @references
#' `r format_bib("aalen_1978")`
#'
#' @export
LearnerCompRisksAalenJohansen = R6Class("LearnerCompRisksAalenJohansen",
  inherit = LearnerCompRisks,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      param_set = ps(
        model = p_lgl(default = FALSE, tags = "train")
      )

      super$initialize(
        id = "cmprsk.aalen",
        param_set = param_set,
        predict_types = "cif",
        feature_types = c("logical", "integer", "numeric", "factor"),
        properties = "weights",
        packages = "survival",
        label = "Aalen Johansen Estimator",
        man = "mlr3proba::mlr_learners_cmprsk.aalen"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")

      if ("weights" %in% task$properties) {
        pv$weights = task$weights$weight
      }

      invoke(survival::survfit,
             formula = task$formula(1),
             data = task$data(cols = task$target_names),
             .args = pv
      )
    },

    .predict = function(task) {
      trans_mat = self$model$pstate
      trans_mat = trans_mat[, -1] # remove (s0) => prob of staying censored (state 0)

      times = self$model$time # unique train set time points
      n_obs = task$nrow # number of test observations
      CIF = stats::setNames(vector("list", ncol(trans_mat)), colnames(trans_mat))

      for (i in seq_along(CIF)) {
        CIF[[i]] = matrix(
          data = rep(trans_mat[, i], times = n_obs),
          nrow = n_obs,
          byrow = TRUE,
          dimnames = list(NULL, times)
        )
      }

      list(cif = CIF)
    }
  )
)

#' @include aaa.R
register_learner("cmprsk.aalen", LearnerCompRisksAalenJohansen)
