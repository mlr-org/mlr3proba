#' @template surv_measure
#' @templateVar title Uno's TPR
#' @templateVar inherit `MeasureSurvAUC`/[MeasureSurv]
#' @templateVar fullname MeasureSurvUnoTPR
#' @templateVar shortname surv.unoTPR
#' @templateVar pars times = 0, lp_thresh = 0
#' @templateVar times_par TRUE
#' @templateVar thresh_par TRUE
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
    initialize = function(times = 0, lp_thresh = 0) {

      assertNumeric(times, len = 1)

      super$initialize(integrated = FALSE,
                       times = times,
                       id = "surv.unoTPR",
                       properties = c("requires_task", "requires_train_set"))

      assertNumeric(lp_thresh, len = 1)
      private$.lp_thresh = lp_thresh
    },

    score_internal = function(prediction, task, train_set, ...) {
      tpr = super$score_internal(prediction = prediction,
                                 task = task,
                                 train_set = train_set,
                                 FUN = survAUC::sens.uno
      )

      tpr[, findInterval(self$lp_thresh, sort(unique(prediction$lp)))]
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
    .lp_thresh = numeric(0)
  )
)
