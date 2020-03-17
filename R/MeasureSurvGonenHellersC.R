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
#' @family lp survival measures
#' @export
MeasureSurvGonenC = R6Class("MeasureSurvGonenC",
  inherit = MeasureSurv,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "surv.gonenC",
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
        predict_type = "lp"
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      survAUC::GHCI(prediction$lp)
    }
  )
)
