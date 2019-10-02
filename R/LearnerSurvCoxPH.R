LearnerSurvCoxPH = R6Class("LearnerSurvCoxPH", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.coxph",
        param_set = ParamSet$new(
          params = list(
            ParamFct$new(id = "ties", default = "efron", levels = c("efron", "breslow", "exact"), tags = "train"),
            ParamLgl$new(id = "singular.ok", default = TRUE, tags = "train"),
            ParamFct$new(id = "type", default = "efron", levels = c("efron", "aalen", "kalbfleisch-prentice"), tags = "predict")
          )
        ),
        predict_types = c("distr","risk"),
        feature_types = c("logical", "integer", "numeric", "factor"),
        properties = c("weights"),
        packages = c("survival", "distr6")
      )
    },

    train_internal = function(task) {
      pv = self$param_set$get_values(tags = "train")
      if ("weights" %in% task$properties) {
        pv$weights = task$weights$weight
      }

      invoke(survival::coxph, formula = task$formula(), data = task$data(), .args = pv, x = TRUE)
    },

    predict_internal = function(task) {
      newdata = task$data(cols = task$feature_names)

      if(any(is.na(data.frame(task$data(cols = task$feature_names)))))
        stop(sprintf("Learner %s on task %s failed to predict: Missing values in new data (line(s) %s)\n",
                     self$id, task$id, which(is.na(data.frame(task$data(cols = task$feature_names))))))


      risk =  predict(self$model, type = "risk", newdata = newdata)
      pv = self$param_set$get_values(tags = "predict")
      fit = invoke(survival::survfit, formula = self$model, newdata = newdata, se.fit = FALSE, .args = pv)

      distr = suppressAll(apply(fit$surv, 2, function(x)
        distr6::WeightedDiscrete$new(data.frame(x = fit$time, cdf = 1 - x),
                                     decorators = c(CoreStatistics, ExoticStatistics))))

      PredictionSurv$new(task = task, distr = distr, risk = risk, lp = log(risk))
    }

    # importance = function() {
    #   # TODO: renames factors and logicals, so the returned names are not valid
    #   if (is.null(self$model)) {
    #     stopf("No model stored")
    #   }
    #   p = summary(self$model)$coefficients[, 5L]
    #   sort(1 - p, decreasing = TRUE)
    # },

    # selected_features = function() {
    #   # TODO: renames factors and logicals, so the returned names are not valid
    #   if (is.null(self$model)) {
    #     stopf("No model stored")
    #   }
    #   beta = coef(self$model)
    #   names(beta)[!is.na(beta)]
    # }
  )
)
