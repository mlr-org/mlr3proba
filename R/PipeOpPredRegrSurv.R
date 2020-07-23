#' @title PipeOpPredRegrSurv
#'
#' @name mlr_pipeops_trafopred_regrsurv
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
#' library("mlr3")
#'
#' # simple example
#' pred = PredictionRegr$new(row_ids = 1:10, truth = 1:10, response = 1:10)
#' po = po("trafopred_regrsurv")
#'
#' # assume no censoring
#' new_pred = po$predict(list(pred = pred, task = NULL))[[1]]
#' print(new_pred)
#'
#' # add censoring
#' task_surv = tsk("rats")
#' task_regr = po("trafotask_survregr", method = "omit")$train(list(input = task_surv))[[1]]
#' learn = lrn("regr.featureless")
#' pred = learn$train(task_regr)$predict(task_regr)
#' po = po("trafopred_regrsurv")
#' new_pred = po$predict(list(pred = pred, task = task_surv))[[1]]
#' new_pred$truth
#' task_surv$truth()
#'
#' @family PipeOps
#' @family Transformation PipeOps
#' @include PipeOpPredTransformer.R
#' @export
PipeOpPredRegrSurv = R6Class("PipeOpPredRegrSurv",
  inherit = PipeOpPredTransformer,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier of the resulting  object.
    #' @param param_vals (`list()`)\cr
    #'   List of hyperparameter settings, overwriting the hyperparameter settings that would
    #'   otherwise be set during construction.
    initialize = function(id = "trafopred_regrsurv") {
      super$initialize(id = id,
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

      PredictionSurv$new(row_ids = input$row_ids, truth = truth,
                         distr = distr, response = input$response)
    }
  )
)

