#' @title Chambless and Diao's AUC
#'
#' @usage NULL
#' @aliases mlr_measures_surv.chamblessAUC
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvChamblessAUC$new()
#' mlr_measures$get("surv.chamblessAUC")
#' msr("surv.chamblessAUC")
#' ```
#'
#' @description
#' Calls [survAUC::AUC.cd()].
#'
#' @details
#' Requires `lp` `predict_type`. \cr
#' Assumes Cox PH model specification.
#'
#' @references
#' Chambless, L. E. and G. Diao (2006).
#' Estimation of time-dependent area under the ROC curve for long-term risk prediction.
#' Statistics in Medicine 25, 3474–3486.
#'
#' @template seealso_measure
#' @export
MeasureSurvChamblessAUC = R6Class("MeasureSurvChamblessAUC",
  inherit = MeasureSurvAUC,
  public = list(
    initialize = function(integrated = TRUE, times) {
      super$initialize(integrated = integrated,
                       times = times,
                       id = "surv.chamblessAUC",
                       properties = c("requires_learner", "requires_task", "requires_train_set"))
    },

    score_internal = function(prediction, learner, task, train_set, ...) {
      super$score_internal(prediction = prediction,
                           learner = learner,
                           task = task,
                           train_set = train_set,
                           FUN = survAUC::AUC.cd)
    }
  )
)
