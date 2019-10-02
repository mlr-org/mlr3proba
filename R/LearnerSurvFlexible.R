#' @include predict.flexsurvreg.R
LearnerSurvFlexible = R6Class("LearnerSurvFlexible", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.flexible",
        param_set = ParamSet$new(
          params = list(
            ParamInt$new(id = "k", default = 1L, lower = 0L, tags = "train"),
            ParamUty$new(id = "knots", tags = "train"),
            ParamUty$new(id = "bknots", tags = "train"),
            ParamFct$new(id ="scale", default = "odds", levels = c("hazard","odds","normal"), tags = "train"),
            ParamFct$new(id ="timescale", default = "log", levels = c("log","identity"), tags = "train"),
            ParamUty$new(id = "inits", tags = "train"),
            ParamUty$new(id = "fixedpars", tags = "train"),
            ParamDbl$new(id = "cl", default = 0.95, lower = 0, upper = 1, tags = "train"),
            ParamInt$new(id = "maxiter", default = 30L, tags = "train"),
            ParamDbl$new(id = "rel.tolerance", default = 1e-09, tags = "train"),
            ParamDbl$new(id = "toler.chol", default = 1e-10, tags = "train"),
            ParamInt$new(id = "outer.max", default = 10L, tags = "train")
          )),
        predict_types = c("distr","risk"),
        feature_types = c("logical", "integer", "factor","numeric"),
        properties = c("weights"),
        packages = c("flexsurv", "survival", "distr6")
        )
      },

    train_internal = function(task) {
      pars_ctrl = c("maxiter","rel.tolerance","toler.chol","outer.max")
      pv = self$param_set$get_values(tags = "train")
      pv = pv[names(pv) %in% pars_ctrl]
      ctrl = invoke(survival::survreg.control, .args = pv)

      pv = self$param_set$get_values(tags = "train")
      pv = pv[!(names(pv) %in% pars_ctrl)]
      pv$sr.control = ctrl

      if(length(pv$k) == 0) pv$k = 1
      if(length(pv$scale) == 0) pv$scale = "odds"

      if ("weights" %in% task$properties)
        pv$weights = task$weights$weight

      invoke(flexsurv::flexsurvspline, formula =  task$formula(task$feature_names), data = task$data(), .args = pv)
      },

    predict_internal = function(task) {

      if(any(is.na(data.frame(task$data(cols = task$feature_names)))))
        stop(sprintf("Learner %s on task %s failed to predict: Missing values in new data (line(s) %s)\n",
                     self$id, task$id, which(is.na(data.frame(task$data(cols = task$feature_names))))))

      pred = predict(self$model, task)

      PredictionSurv$new(task = task, distr = pred$distr, risk = pred$risk)
    }
  )
)
