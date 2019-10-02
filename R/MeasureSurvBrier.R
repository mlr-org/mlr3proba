#' @title Brier Score
#'
#' @usage NULL
#' @aliases mlr_measures_surv.brier
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvBrier$new()
#' mlr_measures$get("surv.brier")
#' msr("surv.brier")
#' ```
#'
#' @description
#' Calculates the brier score, aka squared loss.
#'
#' @template seealso_measure
#' @export
MeasureSurvBrier = R6::R6Class("MeasureSurvBrier",
                            inherit = MeasureSurv,
                            public = list(
                              initialize = function() {
                                super$initialize(
                                  id = "surv.brier",
                                  range = c(0, Inf),
                                  minimize = TRUE,
                                  predict_type = "distr"
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

                                surv = transpose(data.table::rbindlist(list(lapply(prediction$distr,
                                                                            function(x) x$survival(times)))))

                                mean(unlist((ind - surv)^2))
                              }
                            )
)
