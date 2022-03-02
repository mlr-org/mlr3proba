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
    initialize = function() {
      ps = ps(
        integrated = p_lgl(default = TRUE),
        times = p_uty()
      )
      ps$values$integrated = TRUE

      super$initialize(
        id = "surv.hung_auc",
        properties = c("requires_task", "requires_train_set"),
        label = "Hung and Chiang's AUC",
        man = "mlr3proba::mlr_measures_surv.hung_auc",
        param_set = ps
      )
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...) {
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
        task = task,
        train_set = train_set,
        FUN = survAUC::AUC.hc,
        ...)
    }
  )
)
