#' @template surv_measure
#' @templateVar title Harrell's C-Index
#' @templateVar inherit [MeasureSurv]
#' @templateVar fullname MeasureSurvHarrellC
#' @templateVar shortname surv.harrellC
#'
#' @description
#' Calculates Harrell's C, equivalent to the Fortran implementation in \CRANpkg{Hmisc}.
#'
#' @references
#' \cite{mlr3proba}{harrell_1982}
#'
#' @family Concordance survival measures
#' @family crank survival measures
#' @export
MeasureSurvHarrellC = R6Class("MeasureSurvHarrellC",
  inherit = MeasureSurv,
  public = list(
    initialize = function() {
      super$initialize(
        id = "surv.harrellC",
        range = 0:1,
        minimize = FALSE,
        predict_type = "crank"
      )
    },

    score_internal = function(prediction, ...) {
      cindex(prediction$truth, prediction$crank)
    }
  )
)

#' @useDynLib mlr3proba c_cindex
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

  .Call(c_cindex, time, status, x, PACKAGE = "mlr3proba")
}
