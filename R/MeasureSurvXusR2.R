#' @title Xu and O'Quigley's R2 Coefficient
#'
#' @usage NULL
#' @aliases mlr_measures_surv.xusR2
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvXusR2$new()
#' mlr_measures$get("surv.xusR2")
#' msr("surv.xusR2")
#' ```
#'
#' @description
#' Calls [survAUC::XO()].
#'
#' @details
#' Requires `lp` `predict_type`. \cr
#' Assumes Cox PH model specification.
#'
#' @references
#' Xu, R. and J. O'Quigley (1999).
#' A measure of dependence for proportional hazards models.
#' Journal of Nonparametric Statistics 12, 83–107.
#'
#' @template seealso_measure
#' @export
MeasureSurvXusR2 = R6Class("MeasureSurvXusR2",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.xusR2",
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
        predict_type = "lp",
        properties = c("requires_task", "requires_train_set")
      )
    },

    score_internal = function(prediction, task, train_set, ...) {
      surv_train = task$truth(train_set)

      survAUC::XO(surv_train, prediction$lp, rep(0, length(prediction$lp)))
    }
  )
)
