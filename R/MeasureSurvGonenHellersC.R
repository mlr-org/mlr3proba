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
#' `r format_bib("goenen_2005")`
#'
#' @family Concordance survival measures
#' @family lp survival measures
#' @export
MeasureSurvGonenC = R6Class("MeasureSurvGonenC",
  inherit = MeasureSurv,
  public = list(
    #' @description Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      warning("This is now deprecated, use MeasureSurvCindex instead with `weight_meth = 'GH'`.")

      super$initialize(
        id = "surv.gonenC",
        range = 0:1,
        minimize = FALSE,
        packages = "survAUC",
        predict_type = "lp",
        man = "mlr3proba::mlr_measures_surv.gonenC"
      )
    }
  ),

  private = list(
    .score = function(prediction, ...) {
      c_gonen(prediction$lp, 0.5)
    }
  )
)
