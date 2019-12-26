#' @template surv_measure
#' @templateVar title Gonen and Heller's C-Index
#' @templateVar inherit [MeasureSurv]
#' @templateVar fullname MeasureSurvGonenC
#' @templateVar shortname surv.gonenC
#'
#' @description
#' Calls [survAUC::GHCI()].
#'
#' Assumes Cox PH model specification.
#'
#' @template measure_survAUC
#'
#' @references
#' \cite{mlr3proba}{goenen_2005}
#'
#' @family Concordance survival measures
#' @export
MeasureSurvGonenC = R6Class("MeasureSurvGonenC",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.gonenC",
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
        predict_type = "lp"
      )
    },

    score_internal = function(prediction, ...) {
      survAUC::GHCI(prediction$lp)
    }
  )
)
