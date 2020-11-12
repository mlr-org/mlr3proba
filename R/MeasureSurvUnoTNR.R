#' @template surv_measure
#' @templateVar title Uno's TNR
#' @templateVar fullname MeasureSurvUnoTNR
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
#' @template param_times
#' @template param_thresh
#' @template field_thresh
#'
#' @family lp survival measures
#'
#' @references
#' `r format_bib("uno_2007")`
#'
#' @family AUC survival measures
#' @export
MeasureSurvUnoTNR = R6Class("MeasureSurvUnoTNR",
  inherit = MeasureSurvAUC,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(times = 0, lp_thresh = 0) {
      assertNumeric(times, len = 1)

      super$initialize(
        integrated = FALSE,
        times = times,
        id = "surv.uno_tnr",
        man = "mlr3proba::mlr_measures_surv.uno_tnr"
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
    .score = function(prediction, ...) {
      tnr = super$.score(
        prediction = prediction,
        FUN = survAUC::spec.uno,
        task = task
      )

      tnr[, findInterval(self$lp_thresh, sort(unique(prediction$lp)))]
    }
  )
)
