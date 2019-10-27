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
#' @references
#' Brier, G. (1950).
#' Verification of forecasts expressed in terms of probability.
#' Monthly Weather Review, 78(1), 1-3.
#' \doi{10.1175/1520-0493(1950)078<0001:VOFEIT>2.0.CO;2}
#'
#' Graf, E., Schmoor, C., Sauerbrei, W. and Schumacher, M. (1999).
#' Assessment and comparison of prognostic classification schemes for survival data.
#' Statistics in Medicine, 18(17), 2529-2545.
#' \doi{10.1002/(SICI)1097-0258(19990915/30)18:17/18<2529::AID-SIM274>3.0.CO;2-5}
#'
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

                                # Crude estimation of the brier score as the expected value over time
                                # for each observation and a final score as the sample mean over
                                # all observations.

                                ind = matrix(truth, nrow = nr, ncol = nc) >
                                  matrix(times, nrow = nr, ncol = nc, byrow = T)

                                surv = transpose(1 - prediction$distr$cdf(times))

                                mean(unlist((ind - surv)^2))
                              }
                            )
)
