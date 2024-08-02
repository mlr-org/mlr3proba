
PipeOpTaskfSurvClassifIPCW = R6Class(
  "PipeOpTaskfSurvClassifIPCW",
  inherit = mlr3pipelines::PipeOp,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "trafotask_survclassif_IPCW") {
      param_set = ps(
        cutoff_time = p_dbl(0, default = NULL, special_vals = list(NULL))
      )
      super$initialize(
        id = id,
        param_set = param_set,
        input = data.table(
          name    = "input",
          train   = "TaskSurv",
          predict = "TaskSurv"
        ),
        output = data.table(
          name    = "output",
          train   = "TaskClassif",
          predict = "TaskClassif"
        )
      )
    }
  ),

  private = list(
    .predict = function(input) {
      data = input[[1]]$data()
      data$status = factor(data$status, levels = c("0", "1"))
      task = TaskClassif$new(id = input[[1]]$id, backend = data,
                             target = "status", positive = "1")
      list(task)
    },

    .train = function(input) {
      data_trafo = input[[1]]$data()
      cutoff_time = self$param_set$values$cutoff_time
      cutoff_time = 5

      # transform data and calculate weights
      data_trafo$time[data_trafo$time > cutoff_time] = cutoff_time
      data_trafo$status[data_trafo$time == cutoff_time] = 1
      data_trafo$status = (data_trafo$status != 1) * 1

      task_new = TaskSurv$new(id = "ipcw", time = "time", event = "status", backend = data_trafo)
      pred = lrn("surv.kaplan")$train(task_new)$predict(task_new)
      weights = 1 / pred$data$distr[1,]

      # add weights to original data
      data = input[[1]]$data()
      data[["ipc_weights"]] = weights[as.character(data_trafo$time)]
      data[status == 0 & time < cutoff_time, "ipc_weights" := 0]
      data$status = factor(data$status, levels = c("0", "1"))

      # create new task
      task = TaskClassif$new(id = paste0(input[[1]]$id, "_IPCW"), backend = data,
                             target = "status", positive = "1")

      task$set_col_roles("ipc_weights", roles = "weight")

      self$state = list()
      list(task)
    }
  )
)

register_pipeop("trafotask_survclassif_IPCW", PipeOpTaskfSurvClassifIPCW)
