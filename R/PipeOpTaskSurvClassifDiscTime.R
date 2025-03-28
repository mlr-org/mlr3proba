#' @title PipeOpTaskSurvClassifDiscTime
#' @name mlr_pipeops_trafotask_survclassif_disctime
#' @template param_pipelines
#'
#' @description
#' Transform [TaskSurv] to [TaskClassif][mlr3::TaskClassif] by dividing continuous
#' time into multiple time intervals for each observation.
#' This transformation creates a new target variable `disc_status` that indicates
#' whether an event occurred within each time interval.
#' This approach facilitates survival analysis within a classification framework
#' using discrete time intervals (Tutz et al. 2016).
#'
#' @section Dictionary:
#' This [PipeOp][mlr3pipelines::PipeOp] can be instantiated via the
#' [dictionary][mlr3misc::Dictionary] [mlr3pipelines::mlr_pipeops]
#' or with the associated sugar function [mlr3pipelines::po()]:
#' ```
#' PipeOpTaskSurvClassifDiscTime$new()
#' mlr_pipeops$get("trafotask_survclassif_disctime")
#' po("trafotask_survclassif_disctime")
#' ```
#'
#' @section Input and Output Channels:
#' [PipeOpTaskSurvClassifDiscTime] has one input channel named "input", and two
#' output channels, one named "output" and the other "transformed_data".
#'
#' During training, the "output" is the "input" [TaskSurv] transformed to a
#' [TaskClassif][mlr3::TaskClassif].
#' The target column is named `"disc_status"` and indicates whether an event occurred
#' in each time interval.
#' An additional feature named `"tend"` contains the end time point of each interval.
#' Lastly, the "output" task has a column with the original observation ids,
#' under the role `"original_ids"`.
#' The "transformed_data" is an empty [data.table][data.table::data.table].
#'
#' During prediction, the "input" [TaskSurv] is transformed to the "output"
#' [TaskClassif][mlr3::TaskClassif] with `"disc_status"` as target and the `"tend"`
#' feature included.
#' The "transformed_data" is a [data.table] with columns the `"disc_status"`
#' target of the "output" task, the `"id"` (original observation ids),
#' `"obs_times"` (observed times per `"id"`) and `"tend"` (end time of each interval).
#' This "transformed_data" is only meant to be used with the [PipeOpPredClassifSurvDiscTime].
#'
#' @section State:
#' The `$state` contains information about the `cut` parameter used.
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
#' @examplesIf mlr3misc::require_namespaces(c("mlr3pipelines", "mlr3learners"), quietly = TRUE)
#' \dontrun{
#'   library(mlr3)
#'   library(mlr3learners)
#'   library(mlr3pipelines)
#'
#'   task = tsk("lung")
#'
#'   # transform the survival task to a classification task
#'   # all unique event times are used as cutpoints
#'   po_disc = po("trafotask_survclassif_disctime")
#'   task_classif = po_disc$train(list(task))[[1L]]
#'
#'   # the end time points of the discrete time intervals
#'   unique(task_classif$data(cols = "tend"))[[1L]]
#'
#'   # train a classification learner
#'   learner = lrn("classif.log_reg", predict_type = "prob")
#'   learner$train(task_classif)
#' }
#'
#' @references
#' `r format_bib("tutz_2016")`
#'
#' @seealso [pipeline_survtoclassif_disctime]
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
        packages = c("pammtools"),
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
      assert_true(task$cens_type == "right")
      data = task$data()

      if ("disc_status" %in% colnames(task$data())) {
        stop("\"disc_status\" can not be a column in the input data.")
      }

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

      form = formulate(sprintf("Surv(%s, %s)", time_var, event_var), ".")

      long_data = pammtools::as_ped(data = data, formula = form, cut = cut, max_time = max_time)
      self$state$cut = attributes(long_data)$trafo_args$cut
      long_data = as.data.table(long_data)
      setnames(long_data, old = "ped_status", new = "disc_status")
      long_data$disc_status = factor(long_data$disc_status, levels = c("0", "1"))

      # remove some columns from `long_data`
      long_data[, c("offset", "tstart", "interval") := NULL]
      # keep id mapping
      reps = table(long_data$id)
      ids = rep(task$row_ids, times = reps)
      id = NULL
      long_data[, id := ids]

      task_disc = TaskClassif$new(paste0(task$id, "_disc"), long_data,
                                  target = "disc_status", positive = "1")
      task_disc$set_col_roles("id", roles = "original_ids")

      list(task_disc, data.table())
    },

    .predict = function(input) {
      task = input[[1]]
      data = task$data()

      # extract `cut` from `state`
      cut = self$state$cut

      time_var = task$target_names[1]
      event_var = task$target_names[2]

      max_time = max(cut)
      time = data[[time_var]]
      data[[time_var]] = max_time

      status = data[[event_var]]
      data[[event_var]] = 1

      # update form
      form = formulate(sprintf("Surv(%s, %s)", time_var, event_var), ".")

      long_data = as.data.table(pammtools::as_ped(data, formula = form, cut = cut))
      setnames(long_data, old = "ped_status", new = "disc_status")

      disc_status = id = tend = obs_times = NULL # fixing global binding notes of data.table
      long_data[, disc_status := 0]
      # set correct id
      rows_per_id = nrow(long_data) / length(unique(long_data$id))
      long_data$obs_times = rep(time, each = rows_per_id)
      ids = rep(task$row_ids, each = rows_per_id)
      long_data[, id := ids]

      # set correct disc_status
      reps = long_data[, data.table(count = sum(tend >= obs_times)), by = id]$count
      status = rep(status, times = reps)
      long_data[long_data[, .I[tend >= obs_times], by = id]$V1, disc_status := status]
      long_data$disc_status = factor(long_data$disc_status, levels = c("0", "1"))

      # remove some columns from `long_data`
      long_data[, c("offset", "tstart", "interval", "obs_times") := NULL]
      task_disc = TaskClassif$new(paste0(task$id, "_disc"), long_data,
                                  target = "disc_status", positive = "1")
      task_disc$set_col_roles("id", roles = "original_ids")

      # map observed times back
      reps = table(long_data$id)
      long_data$obs_times = rep(time, each = rows_per_id)
      # subset transformed data
      columns_to_keep = c("id", "obs_times", "tend", "disc_status")
      long_data = long_data[, columns_to_keep, with = FALSE]

      list(task_disc, long_data)
    }
  )
)

register_pipeop("trafotask_survclassif_disctime", PipeOpTaskSurvClassifDiscTime)
