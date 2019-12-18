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
#' Gonen, M. and G. Heller (2005).\cr
#' Concordance probability and discriminatory power in proportional hazards regression.\cr
#' Biometrika 92, 965–970.
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
