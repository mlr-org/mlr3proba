#' @title Uno's C-Index
#'
#' @usage NULL
#' @aliases mlr_measures_surv.unos_c
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvUnosC$new()
#' mlr_measures$get("surv.unos_c")
#' msr("surv.unos_c")
#' ```
#'
#' @description
#' Calls [survAUC::UnoC()].
#'
#' @references
#' Uno, H., T. Cai T, M. J. Pencina, R. B. D'Agostino and W. L. Wei. (2011).
#' On the C-statistics for evaluating overall adequacy of risk prediction procedures with censored
#' survival data.
#' Statistics in Medicine. 30(10): 1105–1117.
#' \doi{10.1002/sim.4154}
#'
#' @template seealso_measure
#' @export
MeasureSurvUnosC = R6Class("MeasureSurvUnosC",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.unos_c",
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
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
