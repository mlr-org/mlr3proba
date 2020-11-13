#' @template surv_learner
#' @templateVar title Cox Proportional Hazards
#' @templateVar fullname LearnerSurvCoxPH
#' @templateVar caller [survival::coxph()]
#' @templateVar distr by [survival::survfit.coxph()]
#' @templateVar lp by [survival::predict.coxph()]
#' @description
#'
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
        param_set = ParamSet$new(
          params = list(
            ParamFct$new(id = "ties", default = "efron", levels = c("efron", "breslow", "exact"),
                         tags = "train"),
            ParamLgl$new(id = "singular.ok", default = TRUE, tags = "train"),
            ParamFct$new(id = "type", default = "efron",
                         levels = c("efron", "aalen", "kalbfleisch-prentice"), tags = "predict"),
            ParamInt$new(id = "stype", default = 2L, lower = 1L, upper = 2L, tags = "predict")
          )
        ),
        predict_types = c("distr", "crank", "lp"),
        feature_types = c("logical", "integer", "numeric", "factor"),
        properties = "weights",
        packages = c("survival", "distr6"),
        man = "mlr3proba::mlr_learners_surv.coxph"
      )
    }
  ),

  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")

      if ("weights" %in% task$properties) {
        pv$weights = task$weights$weight
      }

      invoke(survival::coxph, formula = task$formula(), data = task$data(), .args = pv, x = TRUE)
    },

    .predict = function(task) {

      newdata = task$data(cols = task$feature_names)

      # We move the missingness checks here manually as if any NAs are made in predictions then the
      # distribution object cannot be create (initialization of distr6 objects does not handle NAs)
      if (anyMissing(task$data(cols = task$feature_names))) {
        stop(sprintf(
          "Learner %s on task %s failed to predict: Missing values in new data (line(s) %s)\n",
          self$id, task$id,
          paste0(which(is.na(data.frame(task$data(cols = task$feature_names)))), collapse = ", ")))
      }

      pv = self$param_set$get_values(tags = "predict")

      # Get predicted values
      fit = mlr3misc::invoke(survival::survfit, formula = self$model, newdata = newdata,
                             se.fit = FALSE, .args = pv)

      lp = predict(self$model, type = "lp", newdata = newdata)

      .surv_return(times = fit$time, surv = t(fit$surv), lp = lp)
    }
  )
)
