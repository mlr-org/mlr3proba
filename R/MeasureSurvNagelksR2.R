#' @title Nagelkerke's R2 Coefficient
#'
#' @usage NULL
#' @aliases mlr_measures_surv.nagelksR2
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvNagelksR2$new()
#' mlr_measures$get("surv.nagelksR2")
#' msr("surv.nagelksR2")
#' ```
#'
#' @description
#' Calls [survAUC::Nagelk()].
#'
#' @details
#' Requires `lp` `predict_type`. \cr
#' Assumes Cox PH model specification.
#'
#' @references
#' Nagelkerke, N. J. D. (1991).
#' A note on a general definition of the coefficient of determination.
#' Biometrika 78, 691–692.
#'
#' @template seealso_measure
#' @export
MeasureSurvNagelksR2 = R6Class("MeasureSurvNagelksR2",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.nagelksR2",
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
        predict_type = "lp",
        properties = c("requires_task", "requires_train_set")
      )
    },

    score_internal = function(prediction, task, train_set, ...) {
      surv_train = task$truth(train_set)

      survAUC::Nagelk(surv_train, prediction$lp, rep(0, length(prediction$lp)))
    }
  )
)
