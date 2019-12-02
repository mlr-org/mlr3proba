#' @template surv_measure
#' @templateVar title Uno's C-Index
#' @templateVar inherit [MeasureSurv]
#' @templateVar fullname MeasureSurvUnoC
#' @templateVar shortname surv.unoC
#' @templateVar pars times, lp_thresh
#' @templateVar times_par TRUE
#' @templateVar thresh_par TRUE
#'
#' @description
#' Calls [survAUC::UnoC()].
#'
#' Assumes random censoring.
#'
#' @template measure_survAUC
#'
#' @references
#' Uno, H., T. Cai T, M. J. Pencina, R. B. D'Agostino and W. L. Wei. (2011). \cr
#' On the C-statistics for evaluating overall adequacy of risk prediction procedures with censored
#' survival data.\cr
#' Statistics in Medicine. 30(10): 1105–1117.\cr
#' \doi{10.1002/sim.4154}
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
