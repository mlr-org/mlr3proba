#' @title PipeOpDistrCompositor
#'
#' @usage NULL
#' @name mlr_pipeops_distrcompose
#' @format [`R6Class`] inheriting from [`PipeOpCompositor`]/[`PipeOp`].
#'
#' @description
#' Create a survival distribution prediction from two survival learner [`Prediction`][mlr3::Prediction]s,
#' where one predicts a relative risk, and the other estimates the baseline hazard.
#'
#' The resulting `"distr"` prediction is a combination of the the incoming `"risk"` or `"lp"` prediction
#' and incoming `"distr"` prediction. The `model_form` parameter determines what form the composed
#' distribution should assume, this is one of proportional hazards (`ph`), accelerated failure time (`aft`),
#' or proportional odds (`po`). The hazard functions of these are defined respectively by
#' \deqn{PH: h(t) = h0(t)exp(lp)}
#  \deqn{AFT: h(t) = exp(-lp)h0(t/exp(lp))}
#  \deqn{PO: h(t)/h0(t) = {1 + (exp(lp)-1)S-(t)}^-1}
#'
#' @section Construction:
#' ```
#' PipeOpDistrCompositor$new(id = "distrcompose", model_form = "aft", param_vals = list())
#' ```
#' * `id` :: `character(1)`
#'   Identifier of the resulting  object, default `"distrcompose"`.
#' * `model_form` :: `character(1)`
#'   Form of the composed distribution, one of: `aft` (accelerated failure time), `ph`, `po`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [`PipeOpCompositor`]. Instead of a [`Prediction`][mlr3::Prediction], a [`PredictionRegr`][mlr3::PredictionRegr]
#' is used as input and output during prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' The parameters are the parameters inherited from the [`PipeOpCompositor`].
#'
#' @section Internals:
#' Inherits from [`PipeOpCompositor`] by implementing the `private$weighted_avg_predictions()` method.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOpCompositor`]/[`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOpCompositor`]/[`PipeOp`].
#'
#' @family PipeOps
#' @family Ensembles
#' @include PipeOpCompositor.R
#' @examples
#' library("mlr3")
#'
#' # Simple Bagging
#' gr = greplicate(n = 5,
#'   po("subsample") %>>%
#'   po("learner", lrn("classif.rpart"))
#' ) %>>%
#'   po("classifavg")
#'
#' resample(tsk("iris"), GraphLearner$new(gr), rsmp("holdout"))
PipeOpDistrCompositor = R6Class("PipeOpDistrCompositor",
                        inherit = PipeOpCompositor,

                        public = list(
                          initialize = function(id = "distrcompose", param_vals = list(), ...) {
                            super$initialize(innum = 2, id, param_vals = param_vals, prediction_type = "PredictionSurv", ...)
                          }
                        ),
                        private = list(
                          composedistr = function(inputs, row_ids, truth) {
                            print(inputs)
                            response_matrix = simplify2array(map(inputs, "response"))
                            response = c(response_matrix %*% weights)
                            se = NULL

                            PredictionRegr$new(row_ids = row_ids, truth = truth, response = response, se = se)
                          }
                        )
)

mlr3pipelines::mlr_pipeops$add("distrcompose", PipeOpDistrCompositor)
