LearnerSurvKaplanMeier = R6Class("LearnerSurvKaplanMeier", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.kaplan",
        predict_types = "distr",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = c("missings", "importance", "selected_features"),
        packages = c("survival", "distr6")
      )
    },

    train_internal = function(task) {
      invoke(survival::survfit, formula = task$formula(1), data = task$data())
    },

    predict_internal = function(task) {
      surv = c(1, self$model$surv)
      time = c(0, self$model$time)

      distr = suppressAll(
        distr6::WeightedDiscrete$new(data.frame(x = time, cdf = 1 - surv),
                                     decorators = c(CoreStatistics, ExoticStatistics)))

      PredictionSurv$new(task = task, distr = rep(list(distr), task$nrow))
    },

    importance = function() {
      if (is.null(self$model)) {
        stopf("No model stored")
      }
      fn = self$model$features
      named_vector(fn, 0)
    },

    selected_features = function() {
      character(0L)
    }
  )
)
