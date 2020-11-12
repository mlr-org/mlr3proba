#' @template surv_measure
#' @templateVar title Hung and Chiang's AUC
#' @templateVar fullname MeasureSurvHungAUC
#'
#' @description
#' Calls [survAUC::AUC.hc()].
#'
#' Assumes random censoring.
#'
#' @template measure_survAUC
#' @template param_integrated
#' @template param_times
#'
#' @references
#' `r format_bib("hung_2010")`
#'
#' @family AUC survival measures
#' @family lp survival measures
#' @export
MeasureSurvHungAUC = R6Class("MeasureSurvHungAUC",
  inherit = MeasureSurvAUC,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, times) {
      super$initialize(
        integrated = integrated,
        times = times,
        id = "surv.hung_auc",
        properties = c("requires_task", "requires_train_set"),
        man = "mlr3proba::mlr_measures_surv.hung_auc"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...) {
      super$.score(
        prediction = prediction,
        task = task,
        train_set = train_set,
        FUN = survAUC::AUC.hc,
        ...)
    }
  )
)
