#' @title PipeOpTaskRegrSurv
#'
#' @name mlr_pipeops_trafotask_regrsurv
#'
#' @description
#' Transform [TaskRegr] to [TaskSurv].
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [PipeOpTaskTransformer].
#'
#' The output is the input [TaskRegr] transformed to a [TaskSurv].
#'
#' @section State:
#' The `$state` is a named `list` with the `$state` elements inherited from [PipeOpTaskTransformer].
#'
#' @section Parameters:
#' The parameters are
#'
#' * `status :: (numeric(1))`\cr
#' If `NULL` then assumed no censoring in the dataset. Otherwise should be a vector of `0/1`s
#' of same length as the prediction object, where `1` is dead and `0` censored.
#'
#'
#' @examples
#' library("mlr3")
#'
#' task = tsk("boston_housing")
#' po = po("trafotask_regrsurv")
#'
#' # assume no censoring
#' new_task = po$train(list(task_regr = task, task_surv = NULL))[[1]]
#' print(new_task)
#'
#' # predict step is identical
#' new_task = po$predict(list(task_regr = task, task_surv = NULL))[[1]]
#' print(new_task)
#'
#' po$state
#'
#' # add censoring
#' task_surv = tsk("rats")
#' task_regr = po("trafotask_survregr", method = "omit")$train(list(input = task_surv))[[1]]
#' print(task_regr)
#' new_task = po$train(list(task_regr = task_regr, task_surv = task_surv))[[1]]
#' new_task$truth()
#' task_surv$truth()
#'
#' @family PipeOps
#' @family Transformation PipeOps
#' @include PipeOpPredTransformer.R
#' @export
PipeOpTaskRegrSurv = R6Class("PipeOpTaskRegrSurv",
  inherit = PipeOpTaskTransformer,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param id (`character(1)`)\cr
    #'   Identifier of the resulting  object.
    initialize = function(id = "trafotask_regrsurv") {
      super$initialize(id = id,
                       input = data.table(name = c("task_regr", "task_surv"),
                                          train = c("TaskRegr", "*"),
                                          predict = c("TaskRegr", "*")),
                       output = data.table(name = "output", train = "TaskSurv", predict = "TaskSurv")
      )
    },

    predict_internal = function(inputs) {
      list(private$.transform(inputs))
    }
  ),

  private = list(
    .transform = function(input) {

      task_surv = input$task_surv
      input = input$task_regr$clone(deep = TRUE)
      backend = copy(input$data())


      if (!is.null(task_surv)) {
        assert_class(task_surv, "TaskSurv")
        task_surv = task_surv$clone(deep = TRUE)
        backend$status = task_surv$truth()[, 2]
      } else {
        backend$status = 1
      }

      TaskSurv$new(id = input$id, backend = backend, time = input$target_names, event = "status")
    }
  )
)

