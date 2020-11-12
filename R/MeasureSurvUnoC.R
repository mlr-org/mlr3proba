#' @template surv_measure
#' @templateVar title Uno's C-Index
#' @templateVar fullname MeasureSurvUnoC
#'
#' @description
#' Calls [survAUC::UnoC()].
#'
#' Assumes random censoring.
#'
#' @template measure_survAUC
#'
#' @references
#' `r format_bib("uno_2011")`
#'
#' @family Concordance survival measures
#' @family crank survival measures
#' @export
MeasureSurvUnoC = R6Class("MeasureSurvUnoC",
  inherit = MeasureSurv,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      warning("This is now deprecated, use MeasureSurvCindex instead with `weight_meth = 'G/2'`.")

      super$initialize(
        id = "surv.unoC",
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
        predict_type = "crank",
        properties = c("na_score", "requires_task", "requires_train_set"),
        man = "mlr3proba::mlr_measures_surv.unoC"
      )
    }
  ),

  private = list(
    .score = function(prediction, task, train_set, ...) {
      surv_train = task$truth(train_set)
      perf = survAUC::UnoC(surv_train, prediction$truth, prediction$crank)
      if (is.nan(perf)) {
        perf = NA_real_
      }
      perf
    }
  )
)
