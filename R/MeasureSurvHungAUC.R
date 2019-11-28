#' @title Hung and Chiang's AUC
#'
#' @usage NULL
#' @aliases mlr_measures_surv.hungAUC
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvHungAUC$new()
#' mlr_measures$get("surv.hungAUC")
#' msr("surv.hungAUC")
#' ```
#'
#' @description
#' Calls [survAUC::AUC.hc()].
#'
#' @details
#' Requires `lp` `predict_type`. \cr
#' Assumes random censoring.
#'
#' @references
#' Hung, H. and C.-T. Chiang (2010).
#' Estimation methods for time-dependent AUC models with survival data.
#' Canadian Journal of Statistics 38, 8–26.
#'
#' @template seealso_measure
#' @export
MeasureSurvHungAUC = R6Class("MeasureSurvHungAUC",
  inherit = MeasureSurvAUC,
  public = list(
    initialize = function(integrated = TRUE, times) {
      super$initialize(integrated = integrated,
                       times = times,
                       id = "surv.hungAUC",
                       properties = c("requires_task", "requires_train_set"))
    },

    score_internal = function(prediction, task, train_set, ...) {
      super$score_internal(prediction = prediction,
                           task = task,
                           train_set = train_set,
                           FUN = survAUC::AUC.hc,
                           ...)
    }
  )
)
