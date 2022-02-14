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
    initialize = function() {
      ps = ps(
        times = p_dbl(0),
        lp_thresh = p_dbl(default = 0)
      )
      ps$values = list(lp_thresh = 0)

      super$initialize(
        id = "surv.uno_tpr",
        properties = c("requires_task", "requires_train_set"),
        man = "mlr3proba::mlr_measures_surv.uno_tpr",
        param_set = ps
      )
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...) {
      if (is.null(self$param_set$values$times)) {
        stop("`times` must be non-NULL")
      }

      tpr = super$.score(
        prediction = prediction,
        task = task,
        train_set = train_set,
        FUN = survAUC::sens.uno
      )

      tpr[, findInterval(self$param_set$values$lp_thresh, sort(unique(prediction$lp)))]
    }
  )
)
