MeasureSurvBrier = R6::R6Class("MeasureSurvBrier",
                            inherit = MeasureSurv,
                            public = list(
                              initialize = function() {
                                super$initialize(
                                  id = "surv.brier",
                                  range = c(0, Inf),
                                  minimize = TRUE,
                                  predict_type = "distribution"
                          #        task_properties = "twoclass",
                          #        packages = "Metrics"
                                )
                              },

                              score_internal = function(prediction, ...) {

                                truth = prediction$truth[,1]
                                times = sort(unique(truth))
                                nr = length(truth)
                                nc = length(times)

                                ind = matrix(as.numeric(rep(prediction$truth[,1], each = nc) >
                                  rep(times, nr)), nrow = nr, ncol = nc)

                                surv = transpose(data.table::rbindlist(list(lapply(prediction$distribution,
                                                                            function(x) x$survival(times)))))

                                mean(unlist((ind - surv)^2))
                              }
                            )
)
