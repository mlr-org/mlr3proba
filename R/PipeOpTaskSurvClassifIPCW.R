#' @title PipeOpTaskSurvClassifIPCW
#' @name mlr_pipeops_trafotask_survclassif_IPCW
#' @template param_pipelines
#'
#' @description
#' Transform [TaskSurv] to [TaskClassif][mlr3::TaskClassif] using the **I**nverse
#' **P**robability of **C**ensoring **W**eights (IPCW) method by Vock et al. (2016).
#'
#' Let \eqn{T_i} be the observed times (event or censoring) and \eqn{\delta_i}
#' the censoring indicators for each observation \eqn{i} in the training set.
#' The IPCW technique consists of two steps: first we estimate the censoring
#' distribution \eqn{\hat{G}(t)} using the Kaplan-Meier estimator from the
#' training data. Then we calculate the observation weights given a cutoff time
#' \eqn{\tau} as:
#'
#' \deqn{\omega_i = 1/\hat{G}_{min(T_i,\tau)}}
#'
#' Observations that are censored prior to \eqn{\tau} get zero weights, i.e.
#' \eqn{\omega_i = 0}.
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
#' @section Input and Output Channels:
#' [PipeOpTaskSurvClassifIPCW] has one input channel named "input", and two
#' output channels, one named "output" and the other "data".
#'
#' Training transforms the "input" [TaskSurv] to a [TaskClassif][mlr3::TaskClassif],
#' which is the "output".
#' The target column is named `"status"` and indicates whether an event occurred
#' before the cutoff time \eqn{\tau}.
#' The observed times column is removed from the "output" task.
#' The transformed task has the property `"weights"` (the \eqn{\omega_i}).
#' The "data" is `NULL`.
#'
#' During prediction, the "input" [TaskSurv] is transformed to the "output"
#' [TaskClassif][mlr3::TaskClassif] with `"status"` as target (again indicating
#' if the event occurred before the cutoff time).
#' The "data" is a [data.table] containing the observed `times` \eqn{T_i} and
#' censoring indicators/`status` \eqn{\delta_i} of each subject as well as the corresponding
#' `row_ids`.
#' This "data" is only meant to be used with the [PipeOpPredClassifSurvIPCW].
#'
#' @section Parameters:
#' The parameters are
#'
#' * `cutoff_time :: numeric()`\cr
#' Cutoff time for IPCW. Observations with time larger than `cutoff_time` are censored.
#' Should be reasonably smaller than the maximum event time to avoid enormous weights.
#' * `eps :: numeric()`\cr
#' Small value to replace \eqn{G(t) = 0} censoring probabilities to prevent infinite weights.
#'
#' @references
#' `r format_bib("vock_2016")`
#'
#' @family PipeOps
#' @family Transformation PipeOps
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#'
#'   library(mlr3)
#'   library(mlr3pipelines)
#'
#' task = tsk("lung")
#' part = partition(task)
#' task_train = task$clone()$filter(part$train)
#' task_test = task$clone()$filter(part$test)
#' pipe_op = po("trafotask_survclassif_IPCW", cutoff_time = 500)
#' pipe_op$train(list(task_train))
#' pipe_op$predict(list(task_test))
#' }
#' }
#' @export
PipeOpTaskSurvClassifIPCW = R6Class(
  "PipeOpTaskSurvClassifIPCW",
  inherit = mlr3pipelines::PipeOp,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "trafotask_survclassif_IPCW") {
      param_set = ps(
        cutoff_time = p_dbl(0),
        eps = p_dbl(0, default = 1e-3)
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
          name    = c("output", "data"),
          train   = c("TaskClassif", "NULL"),
          predict = c("TaskClassif", "list")
        )
      )
    }
  ),

  private = list(
    .train = function(input) {
      task = input[[1]]

      # checks
      assert_true(task$censtype == "right")
      cutoff_time = assert_numeric(self$param_set$values$cutoff_time, null.ok = FALSE)
      max_event_time = max(task$unique_event_times())
      stopifnot(cutoff_time < max_event_time)

      # G(t): KM estimate of the censoring distribution
      times = task$times()
      status = task$status()
      cens_fit = survival::survfit(Surv(times, 1 - status) ~ 1)
      # make a G(t) one-column matrix => to use in `distr6` function later
      cens_surv = matrix(cens_fit$surv, ncol = 1) # rows => times

      # apply the cutoff to `times`
      cut_times = times
      cut_times[cut_times > cutoff_time] = cutoff_time
      # get G(t) at the observed cutoff'ed times efficiently
      extend_times = getFromNamespace("C_Vec_WeightedDiscreteCdf", ns = "distr6")
      cens_probs = extend_times(cut_times, cens_fit$time, cdf = 1 - cens_surv, FALSE, FALSE)[,1]
      # substitute `eps` for observations: G(t) = 0
      if (any(cens_probs == 0)) {
        warning("At least one t: G(t) = 0, will substitute with eps to avoid very large weights")
        cens_probs[cens_probs == 0] = self$param_set$values$eps
      }

      # calculate the IPC weights
      ipc_weights = 1 / cens_probs

      data = task$data()
      time_var = task$target_names[1]
      status_var = task$target_names[2]

      # add weights to original data
      data[["ipc_weights"]] = ipc_weights
      # zero weights for censored observations before the cutoff time
      ids = status == 0 & times <= cutoff_time
      data[ids, "ipc_weights" := 0]
      # update target: status = 0 after cutoff (remains the same before cutoff)
      status[times > cutoff_time] = 0
      data[[status_var]] = factor(status, levels = c("0", "1"))
      # remove target time variable
      data[[time_var]] = NULL

      # create classification task
      task_ipcw = TaskClassif$new(id = paste0(task$id, "_IPCW"), backend = data,
                                  target = status_var, positive = "1")
      task_ipcw$set_col_roles("ipc_weights", roles = "weight")

      list(task_ipcw, NULL)
    },

    .predict = function(input) {
      task = input[[1]]
      times = task$times()
      status = task$status()
      data = task$data()
      time_var = task$target_names[1]
      status_var = task$target_names[2]
      cutoff_time = assert_numeric(self$param_set$values$cutoff_time, null.ok = FALSE)

      # update target: status = 0 after cutoff (remains the same before cutoff)
      status[times > cutoff_time] = 0
      data[[status_var]] = factor(status, levels = c("0", "1"))
      # remove target time variable
      data[[time_var]] = NULL
      # create classification task
      task_classif = TaskClassif$new(id = paste0(task$id, "_IPCW"), backend = data,
                                     target = "status", positive = "1")

      # keep original row_ids, times and status
      data = list(row_ids = task$row_ids, times = task$times(), status = task$status(),
                  cutoff_time = cutoff_time)
      list(task_classif, data)
    }
  )
)

register_pipeop("trafotask_survclassif_IPCW", PipeOpTaskSurvClassifIPCW)
