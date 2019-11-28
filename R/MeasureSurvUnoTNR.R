#' @title Uno's TNR
#'
#' @usage NULL
#' @aliases mlr_measures_surv.unoTNR
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvUnoTNR$new()
#' mlr_measures$get("surv.unoTNR")
#' msr("surv.unoTNR")
#' ```
#'
#' @description
#' Calls [survAUC::spec.uno()].
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
