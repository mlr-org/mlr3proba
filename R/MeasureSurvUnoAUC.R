#' @title Uno's AUC
#'
#' @usage NULL
#' @aliases mlr_measures_surv.unoAUC
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvUnoAUC$new()
#' mlr_measures$get("surv.unoAUC")
#' msr("surv.unoAUC")
#' ```
#'
#' @description
#' Calls [survAUC::AUC.uno()].
#'
#' @details
#' Requires `lp` `predict_type`. \cr
#' Assumes random censoring.
#'
#' @references
#' Uno, H., T. Cai, L. Tian, and L. J. Wei (2007).
#' Evaluating prediction rules for t-year survivors with censored regression models.
#' Journal of the American Statistical Association 102, 527–537.
#'
#' @template seealso_measure
#' @export
MeasureSurvUnoAUC = R6Class("MeasureSurvUnoAUC",
  inherit = MeasureSurvAUC,
  public = list(
    initialize = function(integrated = TRUE, times) {
      super$initialize(integrated = integrated,
                       times = times,
                       id = "surv.unoAUC",
                       properties = c("requires_task", "requires_train_set"))
    },

    score_internal = function(prediction, task, train_set, ...) {
      super$score_internal(prediction = prediction,
                           task = task,
                           train_set = train_set,
                           FUN = survAUC::AUC.uno,
                           ...)
    }
  )
)
