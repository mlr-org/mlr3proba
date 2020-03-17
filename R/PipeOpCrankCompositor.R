#' @title PipeOpCrankCompositor
#' @aliases mlr_pipeops_crankcompose
#'
#' @description
#' Uses a predicted `distr` in a [PredictionSurv] to estimate (or 'compose') a `crank` prediction.
#'
#' @section Dictionary:
#' This [PipeOp][mlr3pipelines::PipeOp] can be instantiated via the [dictionary][mlr3misc::Dictionary]
#' [mlr3pipelines::mlr_pipeops] or with the associated sugar function [mlr3pipelines::po()]:
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
#'
#' @section Internals:
#' The `median`, `mode`, or `mean` will use analytical expressions if possible but if not they are calculated
#' using [distr6::median.Distribution], [distr6::mode], or [distr6::mean.Distribution] respectively.
#'
#' @section Fields:
#' Only fields inherited from [PipeOp][mlr3pipelines::PipeOp].
#'
#' @section Methods:
#' Only fields inherited from [PipeOp][mlr3pipelines::PipeOp].
#'
#' @seealso [mlr3pipelines::PipeOp] and [crankcompositor]
#' @export
#' @family survival compositors
#' @examples
#' library(mlr3)
#' library(mlr3pipelines)
#' set.seed(1)
#'
#' # Three methods to predict a `crank` from `surv.rpart`
#' task = tgen("simsurv")$generate(30)
#'
#' # Method 1 - Train and predict separately then compose
#' learn = lrn("surv.coxph")$train(task)$predict(task)
#' poc = po("crankcompose", param_vals = list(method = "mean"))
#' poc$predict(list(learn))
#'
#' # Examples not run to save run-time.
#' \dontrun{
#' # Method 2 - Create a graph manually
#' gr = Graph$new()$
#'   add_pipeop(po("learner", lrn("surv.ranger")))$
#'   add_pipeop(po("crankcompose"))$
#'   add_edge("surv.ranger", "crankcompose")
#' gr$train(task)
#' gr$predict(task)
#'
#' # Method 3 - Syntactic sugar: Wrap the learner in a graph
#' ranger.crank = crankcompositor(learner = lrn("surv.ranger"),
#'                             method = "median")
#' resample(task, ranger.crank, rsmp("cv", folds = 2))$predictions()
#' }
PipeOpCrankCompositor = R6Class("PipeOpCrankCompositor",
  inherit = PipeOp,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier of the resulting  object.
    #' @param param_vals (`list()`)\cr
    #'   List of hyperparameter settings, overwriting the hyperparameter settings that would
    #'   otherwise be set during construction.
    initialize = function(id = "crankcompose", param_vals = list(method = "mean")) {
      super$initialize(id = id,
                       param_set = ParamSet$new(params = list(
                         ParamFct$new("method", default = "mean", levels = c("mean","median","mode"), tags = c("predict"))
                       )),
                       param_vals = param_vals,
                       input = data.table(name = "input", train = "NULL", predict = "PredictionSurv"),
                       output = data.table(name = "output", train = "NULL", predict = "PredictionSurv"),
                       packages = "distr6")
      },

    #' @description train_internal
    #' Internal `train` function, will be moved to `private` in a near-future update, should be ignored.
    #' @param inputs
    #' Ignore.
    train_internal = function(inputs) {
      self$state = list()
      list(NULL)
    },

    #' @description predict_internal
    #' Internal `predict` function, will be moved to `private` in a near-future update, should be ignored.
    #' @param inputs
    #' Ignore.
    predict_internal = function(inputs) {
      inpred = inputs[[1]]

      assert("distr" %in% inpred$predict_types)
      method = self$param_set$values$method
      if(length(method) == 0) method = "mean"
      crank = as.numeric(switch(method,
                                median = inpred$distr$median(),
                                mode = inpred$distr$mode(),
                                inpred$distr$mean()
      ))

      if (length(inpred$lp) == 0)
        lp = NULL
      else
        lp = inpred$lp

      return(list(PredictionSurv$new(row_ids = inpred$row_ids, truth = inpred$truth, crank = crank,
                                     distr = inpred$distr, lp = lp)))
    }
  )

  # private = list(
  #   .train = function(inputs) {
  #     self$state = list()
  #     list(NULL)
  #   },
  #
  #   .predict = function(inputs) {
  #     inpred = inputs[[1]]
  #
  #     assert("distr" %in% inpred$predict_types)
  #     method = self$param_set$values$method
  #     if(length(method) == 0) method = "mean"
  #     crank = as.numeric(switch(method,
  #                               median = inpred$distr$median(),
  #                               mode = inpred$distr$mode(),
  #                               inpred$distr$mean()
  #     ))
  #
  #     if (length(inpred$lp) == 0)
  #       lp = NULL
  #     else
  #       lp = inpred$lp
  #
  #     return(list(PredictionSurv$new(row_ids = inpred$row_ids, truth = inpred$truth, crank = crank,
  #                                    distr = inpred$distr, lp = lp)))
  #   }
  # )
)
