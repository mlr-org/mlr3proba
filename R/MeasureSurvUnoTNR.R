#' @template surv_measure
#' @templateVar title Uno's TNR
#' @templateVar fullname MeasureSurvUnoTNR
#' @template measure_survAUC
#' @template param_times2
#' @template param_thresh
#'
#' @description
#' Calls [survAUC::spec.uno()].
#'
#' Assumes random censoring.
#'
#' `times` and `lp_thresh` are arbitrarily set to `0` to prevent crashing, these
#' should be further specified.
#'
#' @references
#' `r format_bib("uno_2007")`
#'
#' @family AUC survival measures
#' @family lp survival measures
#' @export
MeasureSurvUnoTNR = R6Class("MeasureSurvUnoTNR",
  inherit = MeasureSurvAUC,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      ps = ps(
        times = p_dbl(0),
        lp_thresh = p_dbl(default = 0)
      )
      ps$set_values(lp_thresh = 0)

      super$initialize(
        param_set = ps,
        id = "surv.uno_tnr",
        label = "Uno's TNR",
        man = "mlr3proba::mlr_measures_surv.uno_tnr"
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      if (is.null(self$param_set$values$times)) {
        stop("`times` must be non-NULL")
      }

      tnr = super$.score(
        prediction = prediction,
        FUN = survAUC::spec.uno,
        task = task
      )

      tnr[, findInterval(self$param_set$values$lp_thresh, sort(unique(prediction$lp)))]
    }
  )
)

register_measure("surv.uno_tnr", MeasureSurvUnoTNR)
