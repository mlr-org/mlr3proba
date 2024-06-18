#' @title PipeOpTaskSurvClassif
#' @name mlr_pipeops_trafotask_survclassif
#'
#' @description
#' Transform [TaskSurv] to [TaskClassif][mlr3::TaskClassif].
#'
#' @section Input and Output Channels:
#' Input and output channels are inherited from [PipeOpTaskTransformer].
#'
#' The output is the input [TaskSurv] transformed to a [TaskClassif][mlr3::TaskClassif]
#' aswell as the transformed data during prediction.
#'
#' @section Parameters:
#' The parameters are
#'
#' * `cut :: numeric()`\cr
#' Split points, used to partition the data into intervals.
#' If unspecified, all unique event times will be used.
#' * `param max_time :: numeric(1)`\cr
#' If cut is unspecified, this will be the last possible event time.
#' All event times after max_time will be administratively censored at max_time.
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3pipelines)
#'
#'   task = tsk("lung")
#'
#'   if (requireNamespace("mlr3learners", quietly = TRUE)) {
#'     po = po("trafotask_survclassif")
#'     po$train(list(task))
#'     po$predict(list(task))[[1]]
#'   }
#' }
#' }
#' @family PipeOps
#' @family Transformation PipeOps
#' @include PipeOpPredTransformer.R
#' @export
PipeOpTaskSurvClassif = R6Class(
  "PipeOpTaskSurvClassif",
  inherit = PipeOpTaskTransformer,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "trafotask_survclassif", param_vals = list()) {
      ps = ps(
        cut = p_uty(default = NULL),
        max_time = p_dbl(default = NULL, special_vals = list(NULL))
      )
      super$initialize(
        id = id,
        param_set = ps,
        param_vals = param_vals,
        input = data.table::data.table(
          name    = "input",
          train   = "TaskSurv",
          predict = "TaskSurv"
        ),
        output = data.table::data.table(
          name    = c("output", "transformed_data"),
          train   = c("TaskClassif", "data.frame"),
          predict = c("TaskClassif", "data.frame")
        )
      )
    }
  ),

  private = list(
    .train = function(input) {
      task = input[[1]]
      cut = assert_numeric(self$param_set$values$cut, null.ok = TRUE, lower = 0)
      max_time = self$param_set$values$max_time

      time = task$target_names[1]
      event = task$target_names[2]


      formula = mlr3misc::formulate(sprintf("Surv(%s, %s)", time, event), ".")

      # TODO: do without pammtools
      long_data = pammtools::as_ped(data = task$data(), formula = formula, cut = cut, max_time = max_time)
      self$state$attributes = attributes(long_data)$trafo_args[c("formula","cut")]
      long_data = as.data.table(long_data)
      long_data$ped_status = factor(long_data$ped_status, levels = c("0", "1"))

      # remove offset, tstart, interval for dataframe long_data
      long_data[, c("offset", "tstart", "interval") := NULL]

      task = TaskClassif$new(paste0(task$id, "_disc"), long_data, target = "ped_status")
      task$set_col_roles("id", roles = "name")

      list(task, data.frame())
    },

    .predict = function(input) {
      task = input[[1]]
      data = task$data()

      max_time = max(self$state$attributes$cut)
      time = data$time
      data$time = max_time
      data$time2 = time

      new_data = pammtools::as_ped(data, formula = self$state$attributes$formula, cut = self$state$attributes$cut)
      new_data$ped_status = factor(new_data$ped_status, levels = c("0", "1"))

      list(TaskClassif$new(paste0(task$id, "_disc"), new_data, target = "ped_status"),
           new_data)
    }
  )
)

register_pipeop("trafotask_survclassif", PipeOpTaskSurvClassif)
