#' @title PipeOpPredTaskSurvRegr
#'
#' @usage NULL
#' @format Abstract [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Transforms [PredictionSurv] to [TaskRegr][mlr3::TaskRegr].
#'
#' @section Input and Output Channels:
#' [`PipeOpPredTaskSurvRegr`] has one input channel named `"input"` and two output channels named
#' `"task"` and `"pred"`. `"input"` is the [PredictionSurv] to transform, `"task"` is a [TaskSurv]
#' for extracting features from, and `"pred"` is a [PredictionSurv] to transform.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements
#'
#' * `instatus`: Censoring status from input training task.
#' * `outstatus` : Censoring status from input prediction task.
#'
#' @section Parameters:
#' The parameters are
#'
#' * `target::character(1))`\cr
#' Prediction to use for creating the new task. Should be one of: `"response"` (default),
#' `"lp", "crank"`.
#'
#' @section Internals:
#' [PipeOpPredTaskSurvRegr] simply takes the prediction from the [PredictionSurv] and creates a
#' [TaskRegr][mlr3::TaskRegr] with the target specified by the hyper-parameter.
#'
#' @examples
#' library(mlr3)
#' task = tgen("simsurv")$generate(20)
#' pred = lrn("surv.coxph")$train(task)$predict(task)
#' po = po("trafopredtask_survregr", target = "lp")
#' po$train(list(task = task, pred = pred))[[1]]
#' new_task = po$predict(list(task = task, pred = pred))[[1]]
#' print(new_task)
#' new_task$truth() == pred$lp
#'
#' @family PipeOps
#' @family Transformers
#' @export
PipeOpPredTaskSurvRegr = R6Class("PipeOpPredTaskSurvRegr",
  inherit = PipeOpTransformer,
  public = list(
    initialize = function(id = "trafopredtask_survregr", param_vals = list()) {

      ps = ParamSet$new(list(
        ParamFct$new("target", default = "response", levels = c("response", "lp", "crank"))
      ))

      super$initialize(id = id, param_set = ps, param_vals = list(),
                       input = data.table(name = c("task", "pred"), train = c("TaskSurv", "PredictionSurv"),
                                          predict = c("TaskSurv", "PredictionSurv")),
                       output = data.table(name = "output", train = "TaskRegr", predict = "TaskRegr"))
    }
  ),

  private = list(
    .transform = function(inputs) {
      task = inputs$task
      pred = inputs$pred
      target = self$param_set$values$target
      if (is.null(target)) {
        target = "response"
      }
      backend = copy(task$data(cols = task$feature_names))
      backend = cbind(pred[[target]], backend)
      colnames(backend)[1] = target
      TaskRegr$new(id = "tpt_survregr", backend = backend, target = target)
    }
  )
)

