#' @title PipeOpSurvAvg
#' @template param_pipelines
#' @name mlr_pipeops_survavg
#'
#' @description
#' Perform (weighted) prediction averaging from survival [PredictionSurv]s by connecting
#' `PipeOpSurvAvg` to multiple [PipeOpLearner][mlr3pipelines::PipeOpLearner] outputs.
#'
#' The resulting prediction will aggregate any predict types that are contained within all inputs.
#' Any predict types missing from at least one input will be set to `NULL`. These are aggregated
#' as follows:
#' * `"response"`, `"crank"`, and `"lp"` are all a weighted average from the incoming predictions.
#' * `"distr"` is a [distr6::VectorDistribution] containing [distr6::MixtureDistribution]s.
#'
#' Weights can be set as a parameter; if none are provided, defaults to
#' equal weights for each prediction.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [PipeOpEnsemble][mlr3pipelines::PipeOpEnsemble]
#' with a [PredictionSurv] for inputs and outputs.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' The parameters are the parameters inherited from the
#' [PipeOpEnsemble][mlr3pipelines::PipeOpEnsemble].
#'
#' @section Internals:
#' Inherits from [PipeOpEnsemble][mlr3pipelines::PipeOpEnsemble] by implementing the
#' `private$weighted_avg_predictions()` method.
#'
#' @family PipeOps
#' @family Ensembles
#' @export
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#' library(mlr3)
#' library(mlr3pipelines)
#'
#' task = tsk("rats")
#' p1 = lrn("surv.coxph")$train(task)$predict(task)
#' p2 = lrn("surv.kaplan")$train(task)$predict(task)
#' poc = po("survavg", param_vals = list(weights = c(0.2, 0.8)))
#' poc$predict(list(p1, p2))
#' }
#'}
PipeOpSurvAvg = R6Class("PipeOpSurvAvg",
  inherit = mlr3pipelines::PipeOpEnsemble,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param innum `(numeric(1))`\cr
    #'   Determines the number of input channels.
    #'   If `innum` is 0 (default), a vararg input channel is created that can take an arbitrary
    #'   number of inputs.
    #' @param ... `ANY`\cr
    #' Additional arguments passed to [mlr3pipelines::PipeOpEnsemble].
    initialize = function(innum = 0, id = "survavg",
                          param_vals = list(), ...) {
      super$initialize(innum = innum,
                       id = id,
                       param_vals = param_vals,
                       prediction_type = "PredictionSurv",
                       ...)
    }
  ),
  private = list(
    weighted_avg_predictions = function(inputs, weights, row_ids, truth) {
      response_matrix = map(inputs, "response")
      if (some(response_matrix, anyMissing)) {
        response = NULL
      } else {
        response = c(simplify2array(response_matrix) %*% weights)
      }

      crank_matrix = map(inputs, "crank")
      if (some(crank_matrix, anyMissing)) {
        crank = NULL
      } else {
        crank = c(simplify2array(crank_matrix) %*% weights)
      }

      lp_matrix = map(inputs, "lp")
      if (some(lp_matrix, anyMissing)) {
        lp = NULL
      } else {
        lp = c(simplify2array(lp_matrix) %*% weights)
      }

      if (length(unique(weights)) == 1) {
        weights = "uniform"
      }

      distr = map(inputs, "distr")
      if (all(mlr3misc::map_lgl(distr, function(.x)
        checkmate::test_class(.x, "VectorDistribution")))) {
        distr = distr6::mixturiseVector(distr, weights)
      } else {
        distr = NULL
      }

      PredictionSurv$new(row_ids = row_ids, truth = truth,
                         response = response, crank = crank,
                         lp = lp, distr = distr)
    }
  )
)
