#' @title Harrell's C-Index
#'
#' @usage NULL
#' @aliases mlr_measures_surv.harrells_c
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvHarrellsC$new()
#' mlr_measures$get("surv.harrells_c")
#' msr("surv.harrells_c")
#' ```
#'
#' @description
#' Calculates Harrell's C, equivalent to the Fortran implementation in \CRANpkg{Hmisc}.
#'
#' @template seealso_measure
#' @export
MeasureSurvHarrellsC = R6Class("MeasureSurvHarrellsC",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.harrells_c",
        range = 0:1,
        minimize = FALSE
      )
    },

    score_internal = function(prediction, ...) {
      cindex(prediction$truth, prediction$risk)
    }
  )
)

#' @useDynLib mlr3survival c_cindex
cindex = function(S, x) {

  assert_surv(S)
  assert_numeric(x)
  if (anyMissing(S)) {
    return(NA_real_)
  }

  ord = order(S[, 1L])
  time = as.double(S[, 1L])[ord]
  status = as.logical(S[, 2L])[ord]
  x = -as.double(x)[ord]

  .Call(c_cindex, time, status, x, PACKAGE = "mlr3survival")
}
