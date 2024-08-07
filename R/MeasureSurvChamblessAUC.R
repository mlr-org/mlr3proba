#' @template surv_measure
#' @templateVar title Chambless and Diao's AUC
#' @templateVar fullname MeasureSurvChamblessAUC
#' @template measure_survAUC
#' @template param_integrated
#' @template param_times
#'
#' @description
#' Calls [survAUC::AUC.cd()].
#'
#' Assumes Cox PH model specification.
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
        integrated = p_lgl(default = TRUE),
        times = p_uty()
      )
      ps$values$integrated = TRUE

      super$initialize(
        id = "surv.chambless_auc",
        properties = c("requires_learner", "requires_task", "requires_train_set"),
        label = "Chambless and Diao's AUC",
        man = "mlr3proba::mlr_measures_surv.chambless_auc",
        param_set = ps
      )
    }
  ),

  private = list(
    .score = function(prediction, learner, task, train_set, ...) {
      if (!inherits(learner, "LearnerSurvCoxPH")) {
        stop("surv.chambless_auc only compatible with Cox PH models")
      }
      ps = self$param_set$values
      if (!ps$integrated) {
        msg = "If `integrated=FALSE` then `times` should be a scalar numeric."
        assert_numeric(ps$times, len = 1L, .var.name = msg)
      } else {
        if (!is.null(ps$times) && length(ps$times) == 1L) {
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

register_measure("surv.chambless_auc", MeasureSurvChamblessAUC)
