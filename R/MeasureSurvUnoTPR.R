#' @title Uno's TPR
#'
#' @usage NULL
#' @aliases mlr_measures_surv.unoTPR
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvUnoTPR$new()
#' mlr_measures$get("surv.unoTPR")
#' msr("surv.unoTPR")
#' ```
#'
#' @description
#' Calls [survAUC::sens.uno()].
#'
#' @details
#' Requires `lp` `predict_type`. \cr
#' Assumes random censoring.
#'
#' @references
#' Uno, H., T. Cai, L. Tian, and L. J. Wei (2007).
#' Evaluating prediction rules for t-year survivors with censored regression models.
#' Journal of the American Statistical Association 102, 527–537.
#'
#' @template seealso_measure
#' @export
MeasureSurvUnoTPR = R6Class("MeasureSurvUnoTPR",
  inherit = MeasureSurvAUC,
  public = list(
    initialize = function(times, lp_thresh) {
      super$initialize(integrated = FALSE,
                       times = times,
                       id = "surv.unoTPR",
                       properties = c("requires_task", "requires_train_set"))

      if(missing(lp_thresh))
        private$.lp_thresh = numeric(0)
      else {
        assertNumeric(lp_thresh)
        private$.lp_thresh = lp_thresh
      }
    },

    score_internal = function(prediction, task, train_set, ...) {
      tpr = super$score_internal(prediction = prediction,
                           task = task,
                           train_set = train_set,
                           FUN = survAUC::sens.uno
                           )

      if(length(self$lp_thresh) == 0)
        return(list(tpr = tpr, thresh = sort(unique(prediction$lp))))
      else
        return(tpr[, findInterval(self$lp_thresh, sort(unique(prediction$lp)))])
    }
  ),

  active = list(
    lp_thresh = function(lp_thresh) {
      if (missing(lp_thresh)) {
        return(private$.lp_thresh)
      } else {
        assertNumeric(lp_thresh)
        private$.lp_thresh = lp_thresh
      }
    }
  ),

  private = list(
    .lp_thresh = numeric(0)
  )
)
