#' @title Logloss
#'
#' @usage NULL
#' @aliases mlr_measures_surv.logloss
#' @format [R6::R6Class()] inheriting from [MeasureSurv].
#' @include MeasureSurv.R
#'
#' @section Construction:
#' ```
#' MeasureSurvLogloss$new()
#' mlr_measures$get("surv.logloss")
#' msr("surv.logloss")
#' ```
#'
#' @description
#' Calculates the cross-entropy, or logarithmic, loss.
#'
#' @details
#' The logloss, in the context of probabilistic predictions, is defined as the negative log-likelihood
#' evaluated at the observed death-time.
#'
#' @template seealso_measure
#' @export
MeasureSurvLogloss = R6::R6Class("MeasureSurvLogloss",
  inherit = MeasureSurv,
  public = list(
    initialize = function(eps = 1e-15, id = "surv.logloss") {
      super$initialize(
        id = id,
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr",
        packages = "distr6"
      )

      assertNumeric(eps)
      private$.eps <- eps
    },

    score_internal = function(prediction, ...) {
      mean(surv_logloss(prediction$truth, prediction$distr, self$eps))
    },

    eps = function(eps){
      if(is.missing(eps))
        return(private$.eps)
      else {
        assertNumeric(eps)
        private$.eps <- eps
      }
    }
  ),

  private = list(
    .eps = numeric(0)
  )
)
