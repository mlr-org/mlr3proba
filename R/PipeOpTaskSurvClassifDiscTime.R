#' @title PipeOpTaskSurvClassifDiscTime
#' @name mlr_pipeops_trafotask_survclassif_disctime
#' @template param_pipelines
#'
#' @description
#' Transform [TaskSurv] to [TaskClassif][mlr3::TaskClassif] by dividing continuous
#' time into multiple time intervals for each observation.
#' This transformation creates a new target variable `ped_status` that indicates
#' whether an event occurred within each time interval.
#' This approach facilitates survival analysis within a classification framework
#' using discrete time intervals (Tutz et al. 2016).
#'
#' @section Input and Output Channels:
#' [PipeOpTaskSurvClassifDiscTime] has one input channel named "input", and two
#' output channels, one named "output" and the other "transformed_data".
#'
#' During training, the "output" is the "input" [TaskSurv] transformed to a
#' [TaskClassif][mlr3::TaskClassif].
#' The target column is named `ped_status` and indicates whether an event occurred
#' in each time interval.
#' An additional feature named `tend` is added to the ouput task, containing the
#' end time of each interval.
#' The "transformed_data" is an empty [data.table][data.table::data.table].
#'
#' During prediction, the "input" [TaskSurv] is transformed to the "output"
#' [TaskClassif][mlr3::TaskClassif] with `ped_status` as target and the `tend`
#' feature included.
#' The "transformed_data" is a [data.table] which has all the features of the
#' "output" task, including an additional column `time2` containing the
#' original times.
#' This "transformed_data" is only meant to be used with the [PipeOpPredClassifSurvDiscTime].
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
#' Split points, used to partition the data into intervals based on the `time` column.
#' If unspecified, all unique event times will be used.
#' If `cut` is a single integer, it will be interpreted as the number of equidistant
#' intervals from 0 until the maximum event time.
#' * `max_time :: numeric(1)`\cr
#' If `cut` is unspecified, this will be the last possible event time.
#' All event times after `max_time` will be administratively censored at `max_time.`
#' Needs to be greater than the minimum event time in the given task.
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE) &&
#'     requireNamespace("mlr3learners", quietly = TRUE)) {
#'   library(mlr3)
#'   library(mlr3learners)
#'   library(mlr3pipelines)
#'
#'   task = tsk("lung")
#'
#'   # transform the survival task to a classification task
#'   po_disc = po("trafotask_survclassif_disctime", cut = 4)
#'   task_classif = po_disc$train(list(task))[[1L]]
#'
#'   # use a classification learner
#'   learner = lrn("classif.log_reg", predict_type = "prob")
#'   learner$train(task_classif)
#'   learner$predict(task_classif) # does this make sense?
#'
#'   # predict makes sense?
#'   po_disc$predict(list(task))[[1]]
#' }
#' }
#'
#' @references
#' `r format_bib("tutz_2016")`
#'
#' @family PipeOps
#' @family Transformation PipeOps
#' @export
PipeOpTaskSurvClassifDiscTime = R6Class("PipeOpTaskSurvClassifDiscTime",
  inherit = mlr3pipelines::PipeOp,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "trafotask_survclassif_disctime") {
      param_set = ps(
        cut = p_uty(default = NULL),
        max_time = p_dbl(0, default = NULL, special_vals = list(NULL))
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
          name    = c("output", "transformed_data"),
          train   = c("TaskClassif", "data.table"),
          predict = c("TaskClassif", "data.table")
        )
      )
    }
  ),

  private = list(
    .train = function(input) {
      task = input[[1L]]
      assert_true(task$censtype == "right")
      data = task$data()

      cut = assert_numeric(self$param_set$values$cut, null.ok = TRUE, lower = 0)
      max_time = self$param_set$values$max_time

      time_var = task$target_names[1]
      event_var = task$target_names[2]
      if (testInt(cut, lower = 1)) {
        cut = as.integer(seq(0, data[get(event_var) == 1, max(get(time_var))], length.out = cut + 1))
      }

      if (!is.null(max_time)) {
        assert(max_time > data[get(event_var) == 1, min(get(time_var))],
               "max_time must be greater than the minimum event time.")
      }

      form = formulate(sprintf("Surv(%s, %s)", time_var, event_var), ".")

      long_data = pammtools::as_ped(data = data, formula = form, cut = cut, max_time = max_time)
      self$state$cut = attributes(long_data)$trafo_args$cut
      self$state$event_var = event_var
      self$state$time_var = time_var
      long_data = as.data.table(long_data)
      long_data$ped_status = factor(long_data$ped_status, levels = c("0", "1"))

      # remove offset, tstart, interval for dataframe long_data
      long_data[, c("offset", "tstart", "interval") := NULL]

      task_disc = TaskClassif$new(paste0(task$id, "_disc"), long_data,
                                  target = "ped_status", positive = "1")
      task_disc$set_col_roles("id", roles = "name")

      list(task_disc, data.table())
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
      data[[time_var]] = max_time

      status = data[[event_var]]
      data[[event_var]] = 1

      # update form
      form = formulate(sprintf("Surv(%s, %s)", time_var, event_var), ".")

      new_data = pammtools::as_ped(data, formula = form, cut = cut)
      new_data = as.data.table(new_data)

      ped_status = id = NULL # fixing global binding notes of data.table
      new_data[, ped_status := 1]
      new_data[new_data[, .I[.N], by = id]$V1, ped_status := status]
      new_data$ped_status = factor(new_data$ped_status, levels = c("0", "1"))

      # remove offset, tstart, interval for dataframe long_data
      new_data[, c("offset", "tstart", "interval") := NULL]
      task_disc = TaskClassif$new(paste0(task$id, "_disc"), new_data,
                                  target = "ped_status", positive = "1")
      task_disc$set_col_roles("id", roles = "name")

      new_data$time2 = rep(time, each = sum(new_data$id == 1))
      list(task_disc, new_data)
    }
  )
)

register_pipeop("trafotask_survclassif_disctime", PipeOpTaskSurvClassifDiscTime)
