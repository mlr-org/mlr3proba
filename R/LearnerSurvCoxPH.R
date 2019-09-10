LearnerSurvCoxPH = R6Class("LearnerSurvCoxPH", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.coxph",
        param_set = ParamSet$new(
          params = list(
            ParamFct$new(id = "ties", default = "efron", levels = c("efron", "breslow", "exact"), tags = "train")
          )
        ),
        predict_types = "distribution",
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
      fit = invoke(survival::coxph, formula = task$formula(), data = task$data(), .args = pv, x = TRUE)
      set_class(list(fit = fit), "surv.coxph")
    },

    predict_internal = function(task) {
      newdata = task$data(cols = task$feature_names)

      risk =  predict(self$model$fit, type = "risk", newdata = newdata)
      basehaz = survival::basehaz(self$model$fit)

      distribution = lapply(risk, function(x){
        cdf = function(x1){}
        body(cdf) = substitute({
          1 - exp(-(bh[findInterval(x1, time)] * lp))
        }, list(bh = basehaz$hazard, time = basehaz$time, lp = x))

        pdf = function(x1){}
        body(pdf) = substitute({
          1 - exp(-(bh[findInterval(x1, time)] * lp))
        }, list(bh = basehaz$hazard, time = basehaz$time, lp = x))

        suppressMessages(distr6::Distribution$new("coxph", pdf = pdf, cdf = cdf,
                                                  type = Reals$new(zero = TRUE),
                                 support = PosReals$new(zero = TRUE),
                                 valueSupport = "continuous", variateForm = "univariate",
                                 decorators = c(CoreStatistics, ExoticStatistics)))
      })
      PredictionSurv$new(task = task, distribution = distribution)
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
