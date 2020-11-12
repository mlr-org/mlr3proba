#' @template surv_measure
#' @templateVar title Chambless and Diao's AUC
#' @templateVar fullname MeasureSurvChamblessAUC
#'
#' @description
#' Calls [survAUC::AUC.cd()].
#'
#' Assumes Cox PH model specification.
#'
#' @template param_integrated
#' @template param_times
#' @template measure_survAUC
#'
#' @references
#' `r format_bib("chambless_2006")`
#'
#' @family AUC survival measures
#' @family lp survival measures
#' @export
MeasureSurvChamblessAUC = R6Class("MeasureSurvChamblessAUC",
  inherit = MeasureSurvAUC,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, times) {
      super$initialize(
        integrated = integrated,
        times = times,
        id = "surv.chambless_auc",
        properties = c("requires_learner", "requires_task", "requires_train_set"),
        man = "mlr3proba::mlr_measures_surv.chambless_auc"
      )
    }
  ),

  private = list(
    .score = function(prediction, learner, task, train_set, ...) {
      super$.score(
        prediction = prediction,
        learner = learner,
        task = task,
        train_set = train_set,
        FUN = survAUC::AUC.cd)
    }
  )
)
