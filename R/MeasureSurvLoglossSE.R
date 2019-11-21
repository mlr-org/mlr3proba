#' @title Standard Error of Logloss
#'
#' @usage NULL
#' @aliases mlr_measures_surv.logloss_se_se
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvLoglossSESE$new()
#' mlr_measures$get("surv.logloss_se")
#' msr("surv.logloss_se")
#' ```
#'
#' @description
#' Calculates the standard error of the cross-entropy, or logarithmic, loss.
#'
#' @template seealso_measure
#' @export
MeasureSurvLoglossSE = R6::R6Class("MeasureSurvLoglossSE",
                                 inherit = MeasureSurv,
                                 public = list(
                                   initialize = function() {
                                     super$initialize(
                                       id = "surv.logloss_se",
                                       range = c(0, Inf),
                                       minimize = TRUE,
                                       predict_type = "distr"
                                       #       task_properties = "twoclass",
                                       #        packages = "Metrics"
                                     )
                                   },

                                   score_internal = function(prediction, ...) {
                                     times = sort(unique(prediction$truth[,1]))
                                     pred = prediction$distr$pdf(times)
                                     pred[pred == 0] = 1e-15
                                     ll = colMeans(-log(pred))

                                     sd(ll)/sqrt(length(ll))
                                   }
                                 )
)
