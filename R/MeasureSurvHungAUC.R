#' @template surv_measure
#' @templateVar title Hung and Chiang's AUC
#' @templateVar inherit `MeasureSurvAUC`/[MeasureSurv]
#' @templateVar fullname MeasureSurvHungAUC
#' @templateVar shortname surv.hungAUC
#' @templateVar pars integrated = TRUE, times
#' @templateVar int_par TRUE
#' @templateVar times_par TRUE
#'
#' @description
#' Calls [survAUC::AUC.hc()].
#'
#' Assumes random censoring.
#'
#' @template measure_survAUC
#'
#' @references
#' \cite{mlr3proba}{hung_2010}
#'
#' @family AUC survival measures
#' @export
MeasureSurvHungAUC = R6Class("MeasureSurvHungAUC",
  inherit = MeasureSurvAUC,
  public = list(
    initialize = function(integrated = TRUE, times) {
      super$initialize(integrated = integrated,
                       times = times,
                       id = "surv.hungAUC",
                       properties = c("requires_task", "requires_train_set"))
    },

    score_internal = function(prediction, task, train_set, ...) {
      super$score_internal(prediction = prediction,
                           task = task,
                           train_set = train_set,
                           FUN = survAUC::AUC.hc,
                           ...)
    }
  )
)
