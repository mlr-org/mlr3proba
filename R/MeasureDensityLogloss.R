# MeasureDensityLogloss = R6::R6Class("MeasureDensityLogloss",
#                             inherit = MeasureDensity,
#                             public = list(
#                               initialize = function() {
#                                 super$initialize(
#                                   id = "density.logloss",
#                                   range = c(0, Inf),
#                                   minimize = TRUE,
#                                   predict_type = "prob"
#                           #        task_properties = "twoclass",
#                           #        packages = "Metrics"
#                                 )
#                               },
#
#                               score_internal = function(prediction, ...) {
#                                 return(mean(-log(prediction$prob)))
#                               }
#                             )
# )
