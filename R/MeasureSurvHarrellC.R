#' @template surv_measure
#' @templateVar title Harrell's C-Index
#' @templateVar fullname MeasureSurvHarrellC
#'
#' @description
#' Calculates Harrell's C, equivalent to the Fortran implementation in \CRANpkg{Hmisc}.
#'
#' @references
#' `r format_bib("harrell_1982")`
#'
#' @family Concordance survival measures
#' @family crank survival measures
#' @export
MeasureSurvHarrellC = R6Class("MeasureSurvHarrellC",
  inherit = MeasureSurv,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      warning("This is now deprecated, use MeasureSurvCindex instead with the
              function defaults.")
      super$initialize(
        id = "surv.harrellC",
        range = 0:1,
        minimize = FALSE,
        predict_type = "crank",
        man = "mlr3proba::mlr_measures_surv.harrellC"
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      cindex(prediction$truth, prediction$crank)
    }
  )
)
