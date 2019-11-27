#' @title O'Quigley, Xu, and Stare's R2 Coefficient
#'
#' @usage NULL
#' @aliases mlr_measures_surv.oquigleysR2
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvOQuigleysR2$new()
#' mlr_measures$get("surv.oquigleysR2")
#' msr("surv.oquigleysR2")
#' ```
#'
#' @description
#' Calls [survAUC::OXS()].
#'
#' @details
#' Requires `lp` `predict_type`. \cr
#' Assumes Cox PH model specification.
#'
#' @references
#' O'Quigley, J., R. Xu, and J. Stare (2005).
#' Explained randomness in proportional hazards models.
#' Statistics in Medicine 24, 479–489.
#'
#' @template seealso_measure
#' @export
MeasureSurvOQuigleysR2 = R6Class("MeasureSurvOQuigleysR2",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.oquigleysR2",
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
        predict_type = "lp",
        properties = c("requires_task", "requires_train_set")
      )
    },

    score_internal = function(prediction, task, train_set, ...) {
      surv_train = task$truth(train_set)

      survAUC::OXS(surv_train, prediction$lp, rep(0, length(prediction$lp)))
    }
  )
)
