#' @template surv_measure
#' @templateVar title Xu and O'Quigley's R2
#' @templateVar fullname MeasureSurvXuR2
#' @description
#' Calls [survAUC::XO()].
#'
#' Assumes Cox PH model specification.
#'
#' @template measure_survAUC
#'
#' @references
#' \cite{mlr3proba}{xu_1999}
#'
#' @family R2 survival measures
#' @family lp survival measures
#' @export
MeasureSurvXuR2 = R6Class("MeasureSurvXuR2",
  inherit = MeasureSurv,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "surv.xuR2",
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
        predict_type = "lp",
        properties = c("requires_task", "requires_train_set")
      )
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...) {
      surv_train = task$truth(train_set)

      survAUC::XO(surv_train, prediction$lp, rep(0, length(prediction$lp)))
    }
  )
)
