#' @template surv_measure
#' @templateVar title Uno's AUC
#' @templateVar fullname MeasureSurvUnoAUC
#'
#' @description
#' Calls [survAUC::AUC.uno()].
#'
#' Assumes random censoring.
#'
#' @template measure_survAUC
#' @template param_integrated
#' @template param_times
#'
#' @references
#' `r format_bib("uno_2007")`
#'
#' @family AUC survival measures
#' @family lp survival measures
#' @export
MeasureSurvUnoAUC = R6Class("MeasureSurvUnoAUC",
  inherit = MeasureSurvAUC,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(integrated = TRUE, times) {
      ps = ps(
        integrated = p_lgl(default = TRUE),
        times = p_uty()
      )
      ps$values$integrated = TRUE

      super$initialize(
        param_set = ps,
        id = "surv.uno_auc",
        properties = c("requires_task", "requires_train_set"),
        label = "Uno's AUC",
        man = "mlr3proba::mlr_measures_surv.uno_auc"
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

      x = super$.score(
        prediction = prediction,
        task = task,
        train_set = train_set,
        FUN = survAUC::AUC.uno,
        ...
      )
      if (is.list(x)) {
        x$iauc
      } else {
        x
      }
    }
  )
)
