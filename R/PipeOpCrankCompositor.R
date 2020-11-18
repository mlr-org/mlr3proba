#' @title PipeOpCrankCompositor
#' @name mlr_pipeops_compose_crank
#' @template param_pipelines
#'
#' @description
#' Uses a predicted `distr` in a [PredictionSurv] to estimate (or 'compose') a `crank` prediction.
#'
#' @section Dictionary:
#' This [PipeOp][mlr3pipelines::PipeOp] can be instantiated via the
#' [dictionary][mlr3misc::Dictionary] [mlr3pipelines::mlr_pipeops] or with the associated sugar
#' function [mlr3pipelines::po()]:
#' ```
#' PipeOpCrankCompositor$new()
#' mlr_pipeops$get("crankcompose")
#' po("crankcompose")
#' ```
#'
#' @section Input and Output Channels:
#' [PipeOpCrankCompositor] has one input channel named "input", which takes
#' `NULL` during training and [PredictionSurv] during prediction.
#'
#' [PipeOpCrankCompositor] has one output channel named "output", producing `NULL` during training
#' and a [PredictionSurv] during prediction.
#'
#' The output during prediction is the [PredictionSurv] from the "pred" input but with the `crank`
#' predict type overwritten by the given estimation method.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' * `method` :: `character(1)` \cr
#'    Determines what method should be used to produce a continuous ranking from the distribution.
#'    One of `median`, `mode`, or `mean` corresponding to the respective functions in the predicted
#'    survival distribution. Note that for models with a proportional hazards form, the ranking
#'    implied by `mean` and `median` will be identical (but not the value of `crank` itself).
#'    Default is `mean`.
#' * `which` :: `numeric(1)`\cr
#'    If `method = "mode"` then specifies which mode to use if multi-modal, default is the first.
#' * `response` :: `logical(1)`\cr
#'    If `TRUE` then the `response` predict type is estimated with the same values as `crank`.
#' * `overwrite` :: `logical(1)` \cr
#'    If `FALSE` (default) then if the "pred" input already has a `crank`, the compositor only
#'    composes a `response` type if `response = TRUE` and does not already exist. If `TRUE` then
#'    both the `crank` and `response` are overwritten.
#'
#' @section Internals:
#' The `median`, `mode`, or `mean` will use analytical expressions if possible but if not they are
#' calculated using [distr6::median.Distribution], [distr6::mode], or [distr6::mean.Distribution]
#' respectively. `mean` requires \CRANpkg{cubature}.
#'
#' @seealso [pipeline_crankcompositor]
#' @family survival compositors
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3pipelines)
#'   task = tsk("rats")
#'
#'   learn = lrn("surv.coxph")$train(task)$predict(task)
#'   poc = po("crankcompose", param_vals = list(method = "median"))
#'   poc$predict(list(learn))[[1]]
#'
#'   if (requireNamespace("cubature", quietly = TRUE)) {
#'     learn = lrn("surv.coxph")$train(task)$predict(task)
#'     poc = po("crankcompose", param_vals = list(method = "mean"))
#'     poc$predict(list(learn))[[1]]
#'   }
#' }
#' }
#' @export
PipeOpCrankCompositor = R6Class("PipeOpCrankCompositor",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "compose_crank", param_vals = list(method = "mean", response = FALSE,
                                                                  overwrite = FALSE)) {
      ps = ParamSet$new(params = list(
        ParamFct$new("method", default = "mean", levels = c("mean", "median", "mode"),
                     tags = "predict"),
        ParamInt$new("which", default = 1, lower = 1, tags = "predict"),
        ParamLgl$new("response", default = FALSE, tags = "predict"),
        ParamLgl$new("overwrite", default = FALSE, tags = "predict")
      ))
      ps$add_dep("which", "method", CondEqual$new("mode"))

      super$initialize(
        id = id,
        param_set = ps,
        param_vals = param_vals,
        input = data.table(name = "input", train = "NULL", predict = "PredictionSurv"),
        output = data.table(name = "output", train = "NULL", predict = "PredictionSurv"),
        packages = "distr6"
        )
    }
  ),

  private = list(
    .train = function(inputs) {
      self$state = list()
      list(NULL)
    },

    .predict = function(inputs) {

      inpred = inputs[[1]]

      response = self$param_set$values$response
      b_response = !anyMissing(inpred$response)
      if (!length(response)) response = FALSE

      overwrite = self$param_set$values$overwrite
      if (!length(overwrite)) overwrite = FALSE

      # if crank and response already exist and not overwriting then return prediction
      if (!overwrite && (!response || (response && b_response))) {
        return(list(inpred))
      } else {
        assert("distr" %in% inpred$predict_types)
        method = self$param_set$values$method
        if (length(method) == 0) method = "mean"
        if (method == "mean") {
          comp = try(inpred$distr$mean(), silent = TRUE)
          if(class(comp)[1] == "try-error") {
            requireNamespace("cubature")
            comp = try(inpred$distr$mean(cubature = TRUE), silent = TRUE)
          }
          if(class(comp)[1] == "try-error") {
            comp = numeric(length(inpred$crank))
          }
        } else {
          comp = switch(method,
                        median = inpred$distr$median(),
                        mode = inpred$distr$mode(self$param_set$values$which))
        }

        comp = as.numeric(comp)

        # if crank exists and not overwriting then return predicted crank, otherwise compose
        if (!overwrite) {
          crank = inpred$crank
        } else {
          crank = -comp
          # missing imputed with median
          crank[is.na(crank)] = stats::median(crank[!is.na(crank)])
          crank[crank == Inf] = 1e3
          crank[crank == -Inf] = -1e3
        }

        # i) not overwriting or requesting response, and already predicted
        if (b_response && (!overwrite || !response)) {
          response = inpred$response
          # ii) not requesting response and doesn't exist
        } else if (!response) {
          response = NULL
          # iii) requesting response and happy to overwrite
          # iv) requesting response and doesn't exist
        } else {
          response = comp
          response[is.na(response)] = 0
          response[response == Inf | response == -Inf] = 0
        }

        if (!anyMissing(inpred$lp)) {
          lp = inpred$lp
        } else {
          lp = NULL
        }

        return(list(PredictionSurv$new(
          row_ids = inpred$row_ids, truth = inpred$truth, crank = crank,
          distr = inpred$distr, lp = lp, response = response)))
      }
    }
  )
)
