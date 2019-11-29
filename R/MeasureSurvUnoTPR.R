#' @template surv_measure
#' @templateVar title Uno's TPR
#' @templateVar inherit [MeasureSurvAUC]/[MeasureSurv]
#' @templateVar fullname MeasureSurvUnoTPR
#' @templateVar shortname surv.unoTPR
#' @templateVar pars times, lp_thresh
#' @templateVar times_par TRUE
#' @templateVar thresh_par TRUE
#'
#' @description
#' Calls [survAUC::sens.uno()].
#'
#' Assumes random censoring.
#'
#' @references
#' Uno, H., T. Cai, L. Tian, and L. J. Wei (2007).\cr
#' Evaluating prediction rules for t-year survivors with censored regression models.\cr
#' Journal of the American Statistical Association 102, 527–537.\cr
#'
#' @family AUC survival measures
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
