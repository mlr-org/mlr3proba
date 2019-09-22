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
      # set_class(list(fit = fit), "surv.coxph")
    },

    predict_internal = function(task) {
      newdata = task$data(cols = task$feature_names)
      linpred = unname(predict(self$model, newdata = newdata, type = "lp"))
      risk =  predict(self$model, type = "risk", newdata = newdata)
      # Is this correct? Or should basehaz use covariates = 0?
      basehaz = survival::basehaz(self$model)
      colnames(basehaz)[1] = "cumhazard"
      basehaz$hazard = c(basehaz$cumhazard[1], diff(basehaz$cumhazard))
      basehaz = rbind(data.frame(cumhazard = 0, time = 0, hazard = 0), basehaz)


      distr = lapply(risk, function(x){
        cdf = function(x1){}
        body(cdf) = substitute({
          1 - exp(-(bch[findInterval(x1, time, all.inside = TRUE)] * lp))
        }, list(bch = basehaz$cumhazard, time = basehaz$time, lp = x))

        pdf = function(x1){}
        body(pdf) = substitute({
          exp(-(bch[findInterval(x1, time, all.inside = TRUE)] * lp)) * (bh[match(x1, time)] * lp)
        }, list(bch = basehaz$cumhazard, time = basehaz$time, lp = x, bh = basehaz$hazard))

        # Should be discrete not continuous, but need to update distr6 bug
      suppressWarnings(suppressMessages(distr6::Distribution$new(name = "Cox Proportional Hazards",
        short_name = "coxph", pdf = pdf, cdf = cdf, type = Reals$new(zero = TRUE),
        support = PosReals$new(zero = TRUE), valueSupport = "continuous",
        variateForm = "univariate", decorators = c(CoreStatistics, ExoticStatistics))))
       })
      PredictionSurv$new(task = task, distr = distr, risk = linpred)
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
