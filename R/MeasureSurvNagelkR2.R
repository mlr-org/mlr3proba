#' @template surv_measure
#' @templateVar title Nagelkerke's R2
#' @templateVar inherit [MeasureSurv]
#' @templateVar fullname MeasureSurvNagelkR2
#' @templateVar shortname surv.nagelkR2
#' @description
#' Calls [survAUC::Nagelk()].
#'
#' Assumes Cox PH model specification.
#'
#' @template measure_survAUC
#'
#' @references
#' Nagelkerke, N. J. D. (1991). \cr
#' A note on a general definition of the coefficient of determination. \cr
#' Biometrika 78, 691–692.
#'
#' @family R2 survival measures
#' @export
MeasureSurvNagelkR2 = R6Class("MeasureSurvNagelkR2",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.nagelkR2",
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
