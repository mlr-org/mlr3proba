#' @template surv_measure
#' @templateVar title Uno's TNR
#' @templateVar inherit `MeasureSurvAUC`/[MeasureSurv]
#' @templateVar fullname MeasureSurvUnoTNR
#' @templateVar shortname surv.unoTNR
#' @templateVar pars times = 0, lp_thresh = 0
#' @templateVar times_par TRUE
#' @templateVar thresh_par TRUE
#'
#' @description
#' Calls [survAUC::spec.uno()].
#'
#' Assumes random censoring.
#'
#' `times` and `lp_thresh` are arbitrarily set to `0` to prevent crashing, these should be further
#' specified.
#'
#' @template measure_survAUC
#' @family lp survival measures
#'
#' @references
#' \cite{mlr3proba}{uno_2007}
#'
#' @family AUC survival measures
#' @export
MeasureSurvUnoTNR = R6Class("MeasureSurvUnoTNR",
  inherit = MeasureSurvAUC,
  public = list(
    initialize = function(times = 0, lp_thresh = 0) {

      assertNumeric(times, len = 1)

      super$initialize(integrated = FALSE,
                       times = times,
                       id = "surv.unoTNR",
                       properties = character()
                       )

      assertNumeric(lp_thresh, len = 1)
      private$.lp_thresh = lp_thresh
    },

    score_internal = function(prediction, ...) {
      tnr = super$score_internal(prediction = prediction,
                                 FUN = survAUC::spec.uno,
                                 task = task
      )

      tnr[, findInterval(self$lp_thresh, sort(unique(prediction$lp)))]
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
