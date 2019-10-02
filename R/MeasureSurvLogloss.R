#' @title Logloss
#'
#' @usage NULL
#' @aliases mlr_measures_surv.logloss
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvLogloss$new()
#' mlr_measures$get("surv.logloss")
#' msr("surv.logloss")
#' ```
#'
#' @description
#' Calculates the cross-entropy, or logarithmic, loss.
#'
#' @template seealso_measure
#' @export
MeasureSurvLogloss = R6::R6Class("MeasureSurvLogloss",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.logloss",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr"
#       task_properties = "twoclass",
#        packages = "Metrics"
        )
      },

    score_internal = function(prediction, ...) {
      times = sort(unique(prediction$truth[,1]))
      ll = sapply(prediction$distr, function(x){
        pred = x$pdf(x1=times)
        pred[pred == 0] = 1e-5
        return(mean(-log(pred)))
      })
      return(mean(ll))
      }
  )
)
