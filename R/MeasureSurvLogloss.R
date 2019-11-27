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
    initialize = function() {
      super$initialize(
        id = "surv.logloss",
        range = c(0, Inf),
        minimize = TRUE,
        predict_type = "distr"
      )
    },

    score_internal = function(prediction, eps = 1e-15, ...) {
      mean(logloss(prediction$truth, prediction$distr, eps))
    }
  )
)

logloss = function(truth, distribution, eps = 1e-15,...) {
  # get indicator for those not censored
  notcensored = truth[,2] == 1
  # get unique death times for those not censored
  lst = as.list(truth[notcensored, 1])
  names(lst) = paste0("x",1:sum(notcensored))

  # calculate pdf at true death time and set any '0' predictions to a small non-zero value
  pred = as.numeric(do.call(distribution[which(notcensored)]$pdf, lst))
  pred[pred == 0] = eps

  # return negative log-likelihood
  -log(pred)
}
