#' @templateVar title Cox Proportional Hazards
#' @templateVar fullname LearnerSurvCoxPH
#' @templateVar caller [survival::coxph()]
#' @templateVar distr by [survival::survfit.coxph()]
#' @templateVar lp by [survival::predict.coxph()]
#' @templateVar id surv.coxph
#' @template surv_learner
#'
#' @references
#' `r format_bib("cox_1972")`
#'
#' @export
LearnerSurvCoxPH = R6Class("LearnerSurvCoxPH",
  inherit = LearnerSurv,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "surv.coxph",
        param_set = ps(
          ties        = p_fct(default = "efron", levels = c("efron", "breslow", "exact"), tags = "train"),
          singular.ok = p_lgl(default = TRUE, tags = "train"),
          type        = p_fct(default = "efron", levels = c("efron", "aalen", "kalbfleisch-prentice"), tags = "predict"),
          stype       = p_int(1L, 2L, default = 2L, tags = "predict"),
          use_weights = p_lgl(default = FALSE, tags = "train")
        ),
        predict_types = c("crank", "distr", "lp"),
        feature_types = c("logical", "integer", "numeric", "factor"),
        properties = "weights",
        packages = c("survival", "distr6"),
        label = "Cox Proportional Hazards",
        man = "mlr3proba::mlr_learners_surv.coxph"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")

      if (isTRUE(pv$use_weights)) {
        pv$weights = task$weights_learner$weight
      }
      pv$use_weights = NULL

      invoke(survival::coxph, formula = task$formula(), data = task$data(), .args = pv, x = TRUE)
    },

    .predict = function(task) {
      newdata = ordered_features(task, self)
      pv = self$param_set$get_values(tags = "predict")

      # Get survival predictions via `survfit`
      fit = invoke(survival::survfit, formula = self$model, newdata = newdata,
                   se.fit = FALSE, .args = pv)

      # Get linear predictors
      lp = invoke(predict, self$model, type = "lp", newdata = newdata)

      .surv_return(times = fit$time, surv = t(fit$surv), lp = lp)
    }
  )
)

register_learner("surv.coxph", LearnerSurvCoxPH)
