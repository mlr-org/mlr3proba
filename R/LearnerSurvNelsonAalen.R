LearnerSurvNelsonAalen = R6Class("LearnerSurvNelsonAalen", inherit = LearnerSurv,
                                 public = list(
                                   initialize = function() {
                                     super$initialize(
                                       id = "surv.nelson",
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
                                     cumhaz = c(0, self$model$cumhaz)
                                     time = c(0, self$model$time)

                                     distr = suppressAll(
                                       WeightedDiscrete$new(data.frame(x = time, cdf = 1 - exp(-cumhaz)),
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
