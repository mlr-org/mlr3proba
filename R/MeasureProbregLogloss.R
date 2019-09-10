MeasureProbregLogloss = R6::R6Class("MeasureProbregLogloss",
                            inherit = MeasureProbreg,
                            public = list(
                              initialize = function() {
                                super$initialize(
                                  id = "probreg.logloss",
                                  range = c(0, Inf),
                                  minimize = TRUE,
                                  predict_type = "prob"
                          #        task_properties = "twoclass",
                          #        packages = "Metrics"
                                )
                              },

                              score_internal = function(prediction, ...) {
                                return(mean(-log(as.numeric(do.call(prediction$prob$pdf,
                                                                    as.list(prediction$truth))))))
                              }
                            )
)
