LearnerSurv = R6Class("LearnerSurv", inherit = Learner,
  public = list(
    initialize = function(id, param_set = ParamSet$new(), param_vals = list(),
                          predict_types = "distribution", feature_types = character(),
                          properties = character(), packages = character()) {
      super$initialize(id = id, task_type = "surv", param_set = param_set, param_vals = param_vals,
        predict_types = predict_types, feature_types = feature_types, properties = properties,
        packages = packages)
    }
  )
)
