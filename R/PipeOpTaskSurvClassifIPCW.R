#' @title PipeOpTaskSurvClassifIPCW
#' @name mlr_pipeops_trafotask_survclassif_IPCW
#' @template param_pipelines
#'
#' @description
#' Transform [TaskSurv] to [TaskClassif][mlr3::TaskClassif] using IPCW (Vock et al., 2016).
#'
#' @section Dictionary:
#' This [PipeOp][mlr3pipelines::PipeOp] can be instantiated via the
#' [dictionary][mlr3misc::Dictionary] [mlr3pipelines::mlr_pipeops]
#' or with the associated sugar function [mlr3pipelines::po()]:
#' ```
#' PipeOpTaskSurvClassifIPCW$new()
#' mlr_pipeops$get("trafotask_survclassif_IPCW")
#' po("trafotask_survclassif_IPCW")
#' ```
#'
#' @section Parameters:
#' The parameters are
#'
#' * `cutoff_time :: numeric()`\cr
#' Cutoff time for IPCW. Observations with time larger than `cutoff_time` are censored.
#'
#' @references
#' `r format_bib("vock_2016")`
#'
#' @family PipeOps
#' @family Transformation PipeOps
#' @export
PipeOpTaskSurvClassifIPCW = R6Class(
  "PipeOpTaskSurvClassifIPCW",
  inherit = mlr3pipelines::PipeOp,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "trafotask_survclassif_IPCW") {
      param_set = ps(
        cutoff_time = p_dbl(lower = 0, special_vals = list()),
        eps = p_dbl(lower = 0, default = 1e-6)
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
      data = input[[1]]$data()
      time_var = input[[1]]$target_names[1]
      status_var = input[[1]]$target_names[2]

      cutoff_time = self$param_set$values$cutoff_time
      eps = self$param_set$values$eps

      if (cutoff_time >= max(data[[time_var]])) {
        stop("Cutoff time must be smaller than the maximum event time.")
      }

      # transform data and calculate weights
      times = data[[time_var]]
      times[times > cutoff_time] = cutoff_time

      status = data[[status_var]]
      status[times == cutoff_time] = 0

      cens = survival::survfit(Surv(times, 1 - status) ~ 1)
      cens$surv[length(cens$surv)] = cens$surv[length(cens$surv)-1]
      cens$surv[cens$surv == 0] = eps

      weights = rep(1/cens$surv, table(times))

      # add weights to original data
      data[["ipc_weights"]] = weights
      data[status_var == 0 & time_var < cutoff_time, "ipc_weights" := 0]
      data[[status_var]] = factor(data[[status_var]], levels = c("0", "1"))
      data[[time_var]] = NULL

      # create new task
      task = TaskClassif$new(id = paste0(input[[1]]$id, "_IPCW"), backend = data,
                             target = status_var, positive = "1")

      task$set_col_roles("ipc_weights", roles = "weight")

      self$state = list()
      list(task)
    }
  )
)

register_pipeop("trafotask_survclassif_IPCW", PipeOpTaskSurvClassifIPCW)
