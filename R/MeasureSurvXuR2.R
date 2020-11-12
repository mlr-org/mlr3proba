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
#' `r format_bib("xu_1999")`
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
        id = "surv.xu_r2",
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
        predict_type = "lp",
        properties = c("requires_task", "requires_train_set"),
        man = "mlr3proba::mlr_measures_surv.xu_r2"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...) {
      surv_train = task$truth(train_set)

      survAUC::XO(surv_train, prediction$lp, numeric(length(prediction$lp)))
    }
  )
)
