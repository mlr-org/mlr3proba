#' @title PipeOpProbRegr
#' @aliases mlr_pipeops_probregr
#'
#' @description
#' Uses a predicted `distr` in a [PredictionSurv] to estimate (or 'compose') a `crank` prediction.
#'
#' @section Dictionary:
#' This [PipeOp][mlr3pipelines::PipeOp] can be instantiated via the
#' [dictionary][mlr3misc::Dictionary] [mlr3pipelines::mlr_pipeops] or with the associated sugar
#' function [mlr3pipelines::po()]:
#' ```
#' PipeOpProbRegr$new()
#' mlr_pipeops$get("probregr")
#' po("probregr")
#' ```
#'
#' @section Input and Output Channels:
#' [PipeOpProbRegr] has one input channel named "input", which takes
#' `NULL` during training and [PredictionSurv] during prediction.
#'
#' [PipeOpProbRegr] has one output channel named "output", producing `NULL` during training
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
#' The `median`, `mode`, or `mean` will use analytical expressions if possible but if not they are
#' calculated using [distr6::median.Distribution], [distr6::mode], or [distr6::mean.Distribution]
#' respectively.
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
#' task = tsk("boston_housing")
#'
#' # Method 1 - Train and predict separately then compose
#' learn = lrn("regr.featureless", predict_type = "se")
#' pred = learn$train(task)$predict(task)
#' poc = po("probregr")
#' poc$predict(list(pred))
#'
#' # Examples not run to save run-time.
#' \dontrun{
#' # Method 2 - Create a graph manually
#' gr = Graph$new()$
#'   add_pipeop(po("learner", lrn("surv.coxph")))$
#'   add_pipeop(po("probregr"))$
#'   add_edge("surv.coxph", "probregr")
#' gr$train(task)
#' gr$predict(task)
#'
#' # Method 3 - Syntactic sugar: Wrap the learner in a graph
#' cox.crank = crankcompositor(
#'   learner = lrn("surv.coxph"),
#'   method = "median")
#' resample(task, cox.crank, rsmp("cv", folds = 2))$predictions()
#' }
PipeOpProbRegr = R6Class("PipeOpProbRegr",
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
    initialize = function(id = "probregr", param_vals = list(dist = "Normal")) {
      ps = ParamSet$new(params = list(
        ParamFct$new("dist", default = "Normal",
                     levels = listDistributions(filter = list(Tags = "locscale"), simplify = TRUE),
                     tags = "predict")
      ))

      super$initialize(
        id = id,
        param_set = ps,
        param_vals = param_vals,
        input = data.table(name = "input", train = "NULL", predict = "PredictionRegr"),
        output = data.table(name = "output", train = "NULL", predict = "PredictionRegr"),
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
      learner = inputs[[1]]

      assert("se" %in% learner$predict_types)


      pv = self$param_set$values
      dist = pv$dist

      if (is.null(dist) || dist %in% c("Normal")) {
        params = data.table(mean = learner$response, sd = learner$se)
      } else if (dist %in% c("Cauchy", "Gumbel")) {
        params = data.table(location = learner$response, scale = learner$se)
      } else if (dist %in% c("Laplace", "Logistic")) {
        params = data.table(mean = learner$response, scale = learner$se)
      }


      PredictionRegr$new(row_ids = learner$row_ids,
                         truth = learner$truth,
                         response = learner$response,
                         se = learner$se,
                         distr = VectorDistribution$new(distribution = dist, params = params))
    }
  )
)
