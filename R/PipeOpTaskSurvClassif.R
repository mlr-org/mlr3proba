#' @title PipeOpTaskSurvClassif
#' @name mlr_pipeops_trafotask_survclassif
#' @template param_pipelines
#'
#' @description
#' Transform [TaskSurv] to [TaskClassif][mlr3::TaskClassif] by creating multiple
#' interval observations for each subject based on `cut`, with a `ped_status` variable
#' indicating whether an event occurred in each interval.
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [PipeOp][mlr3pipelines::PipeOp].
#'
#' The output is the input [TaskSurv] transformed to a [TaskClassif][mlr3::TaskClassif]
#' as well as the transformed data during prediction.
#'
#' @section State:
#' The `$state` contains information about the `cut` parameter used
#' as well as `time_var` and `event_var`, the names of the two target
#' columns of the survival task.
#'
#' @section Parameters:
#' The parameters are
#'
#' * `cut :: numeric()`\cr
#' Split points, used to partition the data into intervals.
#' If unspecified, all unique event times will be used.
#' If `cut` is a single integer, it will be interpreted as the number of equidistant
#' intervals from 0 until the maximum event time.
#' * `max_time :: numeric(1)`\cr
#' If cut is unspecified, this will be the last possible event time.
#' All event times after max_time will be administratively censored at max_time.
#' Needs to be greater than the minimum event time.
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3pipelines)
#'
#'   task = tsk("lung")
#'   po = po("trafotask_survclassif")
#'   po$train(list(task))
#'   po$predict(list(task))[[1]]
#' }
#' }
#'
#' @references
#' `r format_bib("tutz_2016")`
#'
#' @family PipeOps
#' @family Transformation PipeOps
#' @export
PipeOpTaskSurvClassif = R6Class("PipeOpTaskSurvClassif",
  inherit = mlr3pipelines::PipeOp,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "trafotask_survclassif") {
      param_set = ps(
        cut = p_uty(default = NULL),
        max_time = p_dbl(default = NULL, special_vals = list(NULL))
      )
      super$initialize(
        id = id,
        param_set = param_set,
        input = data.table::data.table(
          name    = "input",
          train   = "TaskSurv",
          predict = "TaskSurv"
        ),
        output = data.table::data.table(
          name    = c("output", "transformed_data"),
          train   = c("TaskClassif", "data.table"),
          predict = c("TaskClassif", "data.table")
        )
      )
    }
  ),

  private = list(
    .train = function(input) {
      task = input[[1]]
      data = task$data()
      assert_true(task$censtype == "right")

      cut = assert_numeric(self$param_set$values$cut, null.ok = TRUE, lower = 0)
      max_time = self$param_set$values$max_time

      time_var = task$target_names[1]
      event_var = task$target_names[2]
      if (testInt(cut, lower = 1)) {
        cut = seq(0, data[get(event_var) == 1, max(get(time_var))], length.out = cut + 1)
      }
      if (!is.null(max_time)) {
        assert(max_time > data[get(event_var) == 1, min(get(time_var))],
               "max_time must be greater than the minimum event time.")
      }

      form = mlr3misc::formulate(sprintf("Surv(%s, %s)", time_var, event_var), ".")

      # TODO: do without pammtools
      long_data = pammtools::as_ped(data = data, formula = form, cut = cut, max_time = max_time)
      self$state$cut = attributes(long_data)$trafo_args$cut
      self$state$event_var = event_var
      self$state$time_var = time_var
      long_data = as.data.table(long_data)
      long_data$ped_status = factor(long_data$ped_status, levels = c("0", "1"))

      # remove offset, tstart, interval for dataframe long_data
      long_data[, c("offset", "tstart", "interval") := NULL]

      task = TaskClassif$new(paste0(task$id, "_disc"), long_data, target = "ped_status", positive = "1")
      task$set_col_roles("id", roles = "name")

      list(task, data.table())
    },

    .predict = function(input) {
      task = input[[1]]
      data = task$data()

      # extract required data from `state`
      cut = self$state$cut
      time_var = self$state$time_var
      event_var = self$state$event_var

      max_time = max(cut)
      time = data[[time_var]]
      data$time = max_time

      # update form
      form = mlr3misc::formulate(sprintf("Surv(%s, %s)", "time", event_var), ".")

      new_data = pammtools::as_ped(data, formula = form, cut = cut)
      new_data = as.data.table(new_data)
      new_data$ped_status = factor(new_data$ped_status, levels = c("0", "1"))

      # remove offset, tstart, interval for dataframe long_data
      new_data[, c("offset", "tstart", "interval") := NULL]
      task = TaskClassif$new(paste0(task$id, "_disc"), new_data, target = "ped_status", positive = "1")
      task$set_col_roles("id", roles = "name")

      new_data$time2 = rep(time, each = sum(new_data$id == 1))
      list(task, new_data)
    }
  )
)

register_pipeop("trafotask_survclassif", PipeOpTaskSurvClassif)
