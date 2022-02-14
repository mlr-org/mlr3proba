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
    initialize = function() {
      ps = ps(
        times = p_uty(),
        integrated = p_lgl(default = TRUE)
      )
      ps$values$integrated = TRUE

      super$initialize(
        id = "surv.chambless_auc",
        properties = c("requires_learner", "requires_task", "requires_train_set"),
        man = "mlr3proba::mlr_measures_surv.chambless_auc",
        param_set = ps
      )
    }
  ),

  private = list(
    .score = function(prediction, learner, task, train_set, ...) {
      ps = self$param_set$values
      if (!ps$integrated) {
        msg = "If `integrated=FALSE` then `times` should be a scalar numeric."
        assert_numeric(ps$times, len = 1, .var.name = msg)
      } else {
        if (!is.null(ps$times) && length(ps$times) == 1) {
          ps$integrated = FALSE
        }
      }

      super$.score(
        prediction = prediction,
        learner = learner,
        task = task,
        train_set = train_set,
        FUN = survAUC::AUC.cd)
    }
  )
)
