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
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3pipelines)
#'
#'   if (requireNamespace("mlr3learners", quietly = TRUE)) {
#'     po = po("trafotask_survclassif")
#'     po$train(list(task))
#'     po$predict(list(task))[[1]]
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
    initialize = function(id = "trafotask_survclassif", param_vals = list()) {
      ps = ps(
        cut = p_uty(default = NULL)
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
          name    = c("output", "meta"),
          train   = c("TaskClassif", "NULL"),
          predict = c("TaskClassif", "data.frame")
        )
      )
    }
  ),

  private = list(
    .train = function(input) {
      task = input[[1]]
      assert_numeric(self$param_set$values$cut, null.ok = TRUE, lower = 0)
      cut = self$param_set$values$cut

      time = task$target_names[1]
      event = task$target_names[2]


      formula = formulate(sprintf("Surv(%s, %s)", time, event), ".")

      # TODO: do without pammtools
      long_data = pammtools::as_ped(data = task$data(), formula, cut = cut)
      self$state$attributes = attributes(long_data)$trafo_args
      long_data = as.data.table(long_data)
      long_data$ped_status = factor(long_data$ped_status)

      # remove offset, tstart, interval for dataframe long_data
      long_data[, c("offset", "tstart", "interval") := NULL]

      task = TaskClassif$new(paste0(task$id, "_disc"), long_data, target = "ped_status")
      task$set_col_roles("id", roles = "name")

      list(task, NULL)
    },

    .predict = function(input) {
      input = input[[1]]
      data = input$data()

      maximum = max(self$state$attributes$cut)
      time = data$time
      data$time = maximum
      data$time2 = time

      new_data = pammtools::as_ped(data, formula = self$state$attributes$formula, cut = self$state$attributes$cut)
      new_data$ped_status = factor(new_data$ped_status)

      list(TaskClassif$new(paste0(input$id, "_disc"), new_data, target = "ped_status"),
           new_data)
    }
  )
)

register_pipeop("trafotask_survclassif", PipeOpTaskSurvClassif)
