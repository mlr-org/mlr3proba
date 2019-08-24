MeasureClassifLogloss = R6::R6Class("MeasureClassifLogloss",
                            inherit = MeasureClassif,
                            public = list(
                              initialize = function() {
                                super$initialize(
                                  id = "classif.logloss",
                                  range = c(0, Inf),
                                  minimize = TRUE,
                                  predict_type = "prob",
                                  task_properties = "twoclass",
                                  packages = "Metrics"
                                )
                              },

                              score_internal = function(prediction, ...) {
                                truth = prediction$truth
                                positive = levels(truth)[1L]
                                Metrics::logLoss(actual = prediction$truth, predicted = prediction$prob)
                              }
                            )
)
