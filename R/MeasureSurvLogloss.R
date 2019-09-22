MeasureSurvLogloss = R6::R6Class("MeasureSurvLogloss",
                               inherit = MeasureSurv,
                               public = list(
                                 initialize = function() {
                                   super$initialize(
                                     id = "surv.logloss",
                                     range = c(0, Inf),
                                     minimize = TRUE,
                                     predict_type = "distribution"
                                     #        task_properties = "twoclass",
                                     #        packages = "Metrics"
                                   )
                                 },

                                 score_internal = function(prediction, ...) {
                                   times = sort(unique(prediction$truth[,1]))
                                   ll = sapply(prediction$distribution, function(x) mean(-log(x$pdf(x1=times))))
                                   return(mean(ll))
                                 }
                               )
)
