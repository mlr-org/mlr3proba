#' @template surv_measure
#' @templateVar title Uno's TNR
#' @templateVar inherit [MeasureSurvAUC]/[MeasureSurv]
#' @templateVar fullname MeasureSurvUnoTNR
#' @templateVar shortname surv.unoTNR
#' @templateVar pars times, lp_thresh
#' @templateVar times_par TRUE
#' @templateVar thresh_par TRUE
#'
#' @description
#' Calls [survAUC::spec.uno()].
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
MeasureSurvUnoTNR = R6Class("MeasureSurvUnoTNR",
  inherit = MeasureSurvAUC,
  public = list(
    initialize = function(times, lp_thresh) {
      super$initialize(integrated = FALSE,
                       times = times,
                       id = "surv.unoTNR",
                       properties = character()
                       )

      if(missing(lp_thresh))
        private$.lp_thresh = numeric(0)
      else {
        assertNumeric(lp_thresh)
        private$.lp_thresh = lp_thresh
      }
    },

    score_internal = function(prediction, ...) {
      tnr = super$score_internal(prediction = prediction,
                                 FUN = survAUC::spec.uno,
                                 task = task
      )

      if(length(self$lp_thresh) == 0)
        return(list(tnr = tnr, thresh = sort(unique(prediction$lp))))
      else
        return(tnr[, findInterval(self$lp_thresh, sort(unique(prediction$lp)))])
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
