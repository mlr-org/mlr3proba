#' @title PipeOpCrankCompositor
#'
#' @usage NULL
#' @name mlr_pipeops_crankcompose
#' @format [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Return a continuous ranking from a survival learner [`PredictionSurv`][PredictionSurv], which
#' predicts `distr`.
#'
#' @section Construction:
#' ```
#' PipeOpCrankCompositor$new(id = "crankcompose", param_vals = list())
#' ```
#' * `id` :: `character(1)` \cr
#'   Identifier of the resulting  object, default `"crankcompose"`.
#' * `param_vals` :: named `list` \cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [PipeOpCrankCompositor] has one input channels named "input", which takes
#' `NULL` during training and [PredictionSurv] during prediction.
#'
#' [PipeOpCrankCompositor] has one output channel named "output", producing `NULL` during training
#' and a [PredictionSurv] during prediction.
#'
#' The output during prediction is the [PredictionSurv] from the "pred" input but with an extra (or overwritten)
#' column for `crank` predict type; which is composed from the `distr` predict type in the [PredictionSurv]
#' object.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' The parameters are:
#' * `method` :: `character(1)` \cr
#'    Determines what method should be used to produce a continuous ranking from the distribution.
#'    One of `median` or `mean` corresponding to the respective functions in the predicted
#'    survival distribution. Note that for models with a proportional hazards form, the ranking
#'    implied by `mean` and `median` will be identical (but not the value of `crank` itself).
#'    Default is `mean`.
#' * `overwrite` :: `logical(1)` \cr
#'    If `FALSE` (default) then if the "input" already has a `crank`, the compositor does nothing
#'    and returns the given [PredictionSurv]. If `TRUE` then the `crank` is overwritten with the `crank`
#'    composed from `distr`.
#'
#' @section Internals:
#' The `median` or `mean` will use analytical expressions if possible but if not they are calculated
#' using [distr6::median.Distribution] or [distr6::mean.Distribution] respectively.
#'
#' @section Fields:
#' Only fields inherited from [PipeOp].
#'
#' @section Methods:
#' Only methods inherited from [PipeOp].
#'
#' @seealso [mlr3pipelines::PipeOp] and [crankcompositor]
#' @export
#' @family survival compositors
#' @examples
#' library("mlr3")
#' library("mlr3pipelines")
#'
#' # Three methods to predict a `crank` from `surv.rpart`
#' task = tsk("rats")
#'
#' # Method 1 - Train and predict separately then compose
#' learn = lrn("surv.coxph")$train(task)$predict(task)
#' poc = po("crankcompose", param_vals = list(method = "mean", overwrite = TRUE))
#' poc$predict(list(learn))
#'
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
PipeOpCrankCompositor = R6Class("PipeOpCrankCompositor",
  inherit = PipeOp,
  public = list(
    initialize = function(id = "crankcompose", param_vals = list()) {
      super$initialize(id = id,
                       param_set = ParamSet$new(params = list(
                         ParamFct$new("method", default = "mean", levels = c("mean","median"), tags = c("predict")),
                         ParamLgl$new("overwrite", default = FALSE, tags = c("predict"))
                       )),
                       param_vals = param_vals,
                       input = data.table(name = "input", train = "NULL", predict = "PredictionSurv"),
                       output = data.table(name = "output", train = "NULL", predict = "PredictionSurv"),
                       packages = "distr6")
      },

    train_internal = function(inputs) {
      self$state = list()
      list(NULL)
      },

    predict_internal = function(inputs) {
      inpred = inputs[[1]]

      overwrite = self$param_set$values$overwrite
      if(length(overwrite) == 0)
        overwrite = FALSE

      if ("crank" %in% inpred$predict_types & !overwrite) {
        return(list(inpred))
      } else {
        assert("distr" %in% inpred$predict_types)

        method = self$param_set$values$method
        if(length(method) == 0)
          method = "mean"
        crank = as.numeric(switch(method,
                                  median = inpred$distr$median(),
                                  inpred$distr$mean()
        ))

        if (length(inpred$lp) == 0)
          lp = NULL
        else
          lp = inpred$lp

        return(list(PredictionSurv$new(row_ids = inpred$row_ids, truth = inpred$truth, crank = crank,
                                distr = inpred$distr, lp = lp)))
      }

    }
  )
)
