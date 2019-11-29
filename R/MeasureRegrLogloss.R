# MeasureRegrLogloss = R6::R6Class("MeasureRegrLogloss",
#                             inherit = MeasureRegr,
#                             public = list(
#                               initialize = function() {
#                                 super$initialize(
#                                   id = "regr.logloss",
#                                   range = c(0, Inf),
#                                   minimize = TRUE,
#                                   predict_type = "distr"
#                           #        task_properties = "twoclass",
#                           #        packages = "Metrics"
#                                 )
#                               },
#
#                               score_internal = function(prediction, ...) {
#                                 return(mean(-log(as.numeric(do.call(prediction$prob$pdf,
#                                                                     as.list(prediction$truth))))))
#                               }
#                             )
# )
