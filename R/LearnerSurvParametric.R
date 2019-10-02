#' @include predict_survreg.R

LearnerSurvParametric = R6Class("LearnerSurvParametric", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.parametric",
        param_set = ParamSet$new(
          params = list(
            ParamFct$new(id = "type", default = "aft", levels = c("aft","ph","odds"), tags = "predict"),
            ParamFct$new(id = "dist", default = "weibull",
                         levels = c("weibull", "exponential", "gaussian", "logistic",
                                    "lognormal","loglogistic"), tags = "train"),
            ParamUty$new(id = "parms", tags = "train"),
            ParamUty$new(id = "init", tags = "train"),
            ParamDbl$new(id = "scale", default = 0, lower = 0, tags = "train"),
            ParamInt$new(id = "maxiter", default = 30L, tags = "train"),
            ParamDbl$new(id = "rel.tolerance", default = 1e-09, tags = "train"),
            ParamDbl$new(id = "toler.chol", default = 1e-10, tags = "train"),
            ParamInt$new(id = "outer.max", default = 10L, tags = "train"),
            ParamLgl$new(id = "robust", default = FALSE, tags = "train"),
            ParamLgl$new(id = "score", default = FALSE, tags = "train")
          )
        ),
        predict_types = c("distr","lp","risk"),
        feature_types = c("logical", "integer", "numeric", "factor"),
        properties = "weights",
        packages = c("survival", "distr6")
      )
    },

    train_internal = function(task) {
      pars_ctrl = c("maxiter","rel.tolerance","toler.chol","outer.max")
      pv = self$param_set$get_values(tags = "train")
      pv = pv[names(pv) %in% pars_ctrl]
      ctrl = invoke(survival::survreg.control, .args = pv)

      pv = self$param_set$get_values(tags = "train")
      pv = pv[!(names(pv) %in% pars_ctrl)]
      pv$control = ctrl

      if ("weights" %in% task$properties) {
        pv$weights = task$weights$weight
      }

      fit = invoke(survival::survreg, formula = task$formula(), data = task$data(), .args = pv)

      location = as.numeric(fit$coefficients[1])
      scale = fit$scale

      basedist = switch(fit$dist,
                 "gaussian" = distr6::Normal$new(mean = location, sd = scale,
                                                 decorators = ExoticStatistics),
                 "weibull" = distr6::Weibull$new(shape = 1/scale, scale = exp(location),
                                                 decorators = ExoticStatistics),
                 "exponential" = distr6::Exponential$new(scale = exp(location),
                                                         decorators = ExoticStatistics),
                 "logistic" = distr6::Logistic$new(mean = location, scale = scale,
                                                   decorators = ExoticStatistics),
                 "lognormal" = distr6::Lognormal$new(meanlog = location, meansd = scale,
                                                     decorators = ExoticStatistics),
                 "loglogistic" = distr6::Loglogistic$new(scale = exp(location),
                                                         shape = 1/scale,
                                                         decorators = ExoticStatistics)
      )

      set_class(list(fit = fit, basedist = basedist), "surv.parametric")
    },

    predict_internal = function(task) {

      if(any(is.na(data.frame(task$data(cols = task$feature_names)))))
        stop(sprintf("Learner %s on task %s failed to predict: Missing values in new data (line(s) %s)\n",
                     self$id, task$id, which(is.na(data.frame(task$data(cols = task$feature_names))))))

      if(length(self$param_set$values$type) == 0) self$param_set$values$type = "ph"

      pred = predict_survreg(self$model, task, self$param_set$values$type, "all")

      PredictionSurv$new(task = task, distr = pred$distr, risk = pred$risk, lp = pred$lp)
    }
  )
)
