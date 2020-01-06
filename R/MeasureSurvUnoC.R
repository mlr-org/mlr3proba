#' @template surv_measure
#' @templateVar title Uno's C-Index
#' @templateVar inherit [MeasureSurv]
#' @templateVar fullname MeasureSurvUnoC
#' @templateVar shortname surv.unoC
#'
#' @description
#' Calls [survAUC::UnoC()].
#'
#' Assumes random censoring.
#'
#' @template measure_survAUC
#'
#' @references
#' \cite{mlr3proba}{uno_2011}
#'
#' @family Concordance survival measures
#' @export
MeasureSurvUnoC = R6Class("MeasureSurvUnoC",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.unoC",
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
        predict_type = "crank",
        properties = c("na_score", "requires_task", "requires_train_set")
      )
    },

    score_internal = function(prediction, task, train_set, ...) {
      surv_train = task$truth(train_set)
      perf = survAUC::UnoC(surv_train, prediction$truth, prediction$crank)
      if (is.nan(perf)) {
        perf = NA_real_
      }
      perf
    }
  )
)
