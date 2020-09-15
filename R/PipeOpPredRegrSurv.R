#' @title PipeOpPredRegrSurv
#' @name mlr_pipeops_trafopred_regrsurv
#' @template param_pipelines
#'
#' @description
#' Transform [PredictionRegr] to [PredictionSurv].
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [PipeOpPredTransformer].
#'
#' The output is the input [PredictionRegr] transformed to a [PredictionSurv]. Censoring can be
#' added with the `status` hyper-parameter. `se` is ignored.
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [`PipeOpPredTransformer`].
#'
#' @section Parameters:
#' The parameters are
#'
#' * `status :: (numeric(1))`\cr
#' If `NULL` then assumed no censoring in the dataset. Otherwise should be a vector of `0/1`s
#' of same length as the prediction object, where `1` is dead and `0` censored.
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#' library(mlr3)
#' library(mlr3pipelines)
#'
#' # simple example
#' pred = PredictionRegr$new(row_ids = 1:10, truth = 1:10, response = 1:10)
#' po = po("trafopred_regrsurv")
#'
#' # assume no censoring
#' new_pred = po$predict(list(pred = pred, task = NULL))[[1]]
#' po$train(list(NULL, NULL))
#' print(new_pred)
#'
#' # add censoring
#' task_surv = tsk("rats")
#' task_regr = po("trafotask_survregr", method = "omit")$train(list(task_surv, NULL))[[1]]
#' learn = lrn("regr.featureless")
#' pred = learn$train(task_regr)$predict(task_regr)
#' po = po("trafopred_regrsurv")
#' new_pred = po$predict(list(pred = pred, task = task_surv))[[1]]
#' all.equal(new_pred$truth, task_surv$truth())
#' }
#' }
#' @family PipeOps
#' @family Transformation PipeOps
#' @include PipeOpPredTransformer.R
#' @export
PipeOpPredRegrSurv = R6Class("PipeOpPredRegrSurv",
  inherit = PipeOpPredTransformer,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "trafopred_regrsurv", param_vals = list()) {

      ps = ParamSet$new(list(
        ParamFct$new("target_type", default = "response", levels = c("crank", "response", "lp"))
      ))

      super$initialize(id = id,
                       param_set = ps,
                       param_vals = param_vals,
                       input = data.table(name = c("pred", "task"), train = "NULL",
                                          predict = c("PredictionRegr", "*")),
                       output = data.table(name = "output", train = "NULL", predict = "PredictionSurv")
      )
    }
  ),

  private = list(
    .transform = function(input) {
      task = input$task
      input = input$pred$clone(deep = TRUE)

      if (!is.null(task)) {
        assert_class(task, "TaskSurv")
        task = task$clone(deep = TRUE)
        truth = task$truth()
      } else {
        truth = Surv(input$truth)
      }

      distr = try(input$distr, silent = TRUE)
      if (class(distr)[1] == "try-error" || is.null(distr)) {
        distr = NULL
      }

      response = lp = NULL
      target_type = self$param_set$values$target_type
      if (is.null(target_type) || target_type == "response") {
        response = input$response
      } else  if (target_type == "lp") {
        lp = input$response
      }

      PredictionSurv$new(row_ids = input$row_ids, truth = truth,
                         distr = distr, crank = input$response, response = response,
                         lp = lp)
    }
  )
)

