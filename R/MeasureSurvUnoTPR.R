#' @template surv_measure
#' @templateVar title Uno's TPR
#' @templateVar fullname MeasureSurvUnoTPR
#'
#' @description
#' Calls [survAUC::sens.uno()].
#'
#' Assumes random censoring.
#'
#' `times` and `lp_thresh` are arbitrarily set to `0` to prevent crashing, these should be further
#' specified.
#'
#' @template measure_survAUC
#' @template param_times
#' @template param_thresh
#' @template field_thresh
#'
#' @references
#' `r format_bib("uno_2007")`
#'
#' @family AUC survival measures
#' @family lp survival measures
#' @export
MeasureSurvUnoTPR = R6Class("MeasureSurvUnoTPR",
  inherit = MeasureSurvAUC,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(times = 0, lp_thresh = 0) {
      assertNumeric(times, len = 1)

      super$initialize(
        integrated = FALSE,
        times = times,
        id = "surv.uno_tpr",
        properties = c("requires_task", "requires_train_set"),
        man = "mlr3proba::mlr_measures_surv.uno_tpr"
      )

      assertNumeric(lp_thresh, len = 1)
      private$.lp_thresh = lp_thresh
    }
  ),

  active = list(
    lp_thresh = function(lp_thresh) {
      if (missing(lp_thresh)) {
        return(private$.lp_thresh)
      } else {
        assertNumeric(lp_thresh, len = 1)
        private$.lp_thresh = lp_thresh
      }
    }
  ),

  private = list(
    .lp_thresh = numeric(0),
    .score = function(prediction, task, train_set, ...) {
      tpr = super$.score(
        prediction = prediction,
        task = task,
        train_set = train_set,
        FUN = survAUC::sens.uno
      )

      tpr[, findInterval(self$lp_thresh, sort(unique(prediction$lp)))]
    }
  )
)
