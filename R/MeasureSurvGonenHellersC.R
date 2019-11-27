#' @title Gonen and Heller's C-Index
#'
#' @usage NULL
#' @aliases mlr_measures_surv.gonensC
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvGonensC$new()
#' mlr_measures$get("surv.gonensC")
#' msr("surv.gonensC")
#' ```
#'
#' @description
#' Calls [survAUC::GHCI()].
#'
#' @details
#' Requires `lp` `predict_type`. \cr
#' Assumes Cox PH model specification.
#'
#' @references
#' Gonen, M. and G. Heller (2005).
#' Concordance probability and discriminatory power in proportional hazards regression.
#' Biometrika 92, 965–970.
#'
#' @template seealso_measure
#' @export
MeasureSurvGonensC = R6Class("MeasureSurvGonensC",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.gonensC",
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
