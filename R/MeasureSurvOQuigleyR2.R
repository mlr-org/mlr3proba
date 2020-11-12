#' @template surv_measure
#' @templateVar title O'Quigley, Xu, and Stare's R2
#' @templateVar fullname MeasureSurvOQuigleyR2
#' @description
#' Calls [survAUC::OXS()].
#'
#' Assumes Cox PH model specification.
#'
#' @template measure_survAUC
#'
#' @references
#' `r format_bib("oquigley_2005")`
#'
#' @family R2 survival measures
#' @family lp survival measures
#' @export
MeasureSurvOQuigleyR2 = R6Class("MeasureSurvOQuigleyR2",
  inherit = MeasureSurv,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "surv.oquigley_r2",
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
        predict_type = "lp",
        properties = c("requires_task", "requires_train_set"),
        man = "mlr3proba::mlr_measures_surv.oquigley_r2"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...) {
      surv_train = task$truth(train_set)

      survAUC::OXS(surv_train, prediction$lp, rep(0, length(prediction$lp)))
    }
  )
)
