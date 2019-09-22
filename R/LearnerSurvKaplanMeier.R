LearnerSurvKaplanMeier = R6Class("LearnerSurvKaplanMeier", inherit = LearnerSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.km",
        predict_types = "distr",
        feature_types = c("logical", "integer", "numeric", "character", "factor", "ordered"),
        properties = c("missings", "importance", "selected_features"),
        packages = c("survival", "distr6")
      )
    },

    train_internal = function(task) {
      fit = invoke(survival::survfit, formula = task$formula(1), data = task$data())
      set_class(list(fit = fit), "surv.km")
    },

    predict_internal = function(task) {
      surv = c(1, self$model$fit$surv)
      time = c(0, self$model$fit$time)

      cdf = function(x1){}
      body(cdf) = substitute(1 - surv[findInterval(x1, time, all.inside = TRUE)])

      distr = suppressMessages(distr6::Distribution$new("Kaplan-Meier","km", cdf = cdf,
                                                type = PosReals$new(zero = TRUE),
                                                support = PosReals$new(zero = TRUE),
                                                valueSupport = "continuous",
                                                variateForm = "univariate",
                                                decorators = c(CoreStatistics,
                                                               ExoticStatistics)))

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
