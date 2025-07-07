#' @title PipeOpTaskSurvRegrPEM
#' @name mlr_pipeops_trafotask_survregr_pem
#' @template param_pipelines
#'
#' @description
#' Transform [TaskSurv] to [TaskRegr][mlr3::TaskRegr] by dividing continuous
#' time into multiple time intervals for each observation.
#' This transformation creates a new target variable `pem_status` that indicates
#' whether an event occurred within each time interval.
#'
#' @section Dictionary:
#' This [PipeOp][mlr3pipelines::PipeOp] can be instantiated via the
#' [dictionary][mlr3misc::Dictionary] [mlr3pipelines::mlr_pipeops]
#' or with the associated sugar function [mlr3pipelines::po()]:
#' ```
#' PipeOpTaskSurvRegrPEM$new()
#' mlr_pipeops$get("trafotask_survregr_pem")
#' po("trafotask_survregr_pem")
#' ```
#'
#' @section Input and Output Channels:
#' [PipeOpTaskSurvRegrPEM] has one input channel named "input", and two
#' output channels, one named "output" and the other "transformed_data".
#'
#' During training, the "output" is the "input" [TaskSurv] transformed to a
#' [TaskRegr][mlr3::TaskRegr].
#' The target column is named `"pem_status"` and indicates whether an event occurred
#' in each time interval.
#' An additional numeric feature named `"tend"` contains the end time point of each interval.
#' Lastly, the "output" task has an offset column `"offset"`.
#' The offset, also referred to as *exposure*, is the **logarithm of time spent in interval** \eqn{j}, i.e. \eqn{log(t_j)}.
#' The "transformed_data" is an empty [data.table][data.table::data.table].
#'
#' During prediction, the "input" [TaskSurv] is transformed to the "output"
#' [TaskRegr][mlr3::TaskRegr] with `"pem_status"` as target, `"tend"` included as feature and
#' and the `"offset"` column which is assigned the offset `"col_role"`.
#' The "transformed_data" is a [data.table][data.table::data.table] with columns the `"pem_status"`
#' target of the "output" task, the `"id"` (original observation ids),
#' `"obs_times"` (observed times per `"id"`) and `"tend"` (end time of each interval).
#' This "transformed_data" is only meant to be used with the [PipeOpPredRegrSurvPEM].
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
#' @examplesIf (mlr3misc::require_namespaces(c("mlr3pipelines", "mlr3extralearners"), quietly = TRUE))
#' \dontrun{
#'   library(mlr3)
#'   library(mlr3learners)
#'   library(mlr3pipelines)
#'
#'   task = tsk("lung")
#'
#'   # transform the survival task to a regression task
#'   # all unique event times are used as cutpoints
#'   po_pem = po("trafotask_survregr_pem")
#'   task_regr = po_pem$train(list(task))[[1L]]
#'
#'   # the end time points of the discrete time intervals
#'   unique(task_regr$data(cols = "tend")[[1L]])
#'
#'   # train a regression learner that supports poisson regression
#'   # e.g. regr.gam
#'   # won't run unless learner can accept offset column role
#'   learner = lrn("regr.gam", formula = pem_status ~ s(age) + s(tend), family = "poisson")
#'   learner$train(task_regr)
#'
#'   # e.g. regr.xgboost, note prior data processing steps
#'   learner = as_learner(
#'     po("modelmatrix", formula = ~ as.factor(tend) + .) %>>%
#'     lrn("regr.xgboost", objective = "count:poisson", nrounds = 100, eta = 0.1)
#'   )
#'   learner$train(task_regr)
#'   }
#'
#' @references
#' `r format_bib("bender_2018")`
#'
#' @seealso [pipeline_survtoregr_pem]
#' @family PipeOps
#' @family Transformation PipeOps
#' @export
PipeOpTaskSurvRegrPEM = R6Class("PipeOpTaskSurvRegrPEM",
  inherit = mlr3pipelines::PipeOp,

  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id = "trafotask_survregr_pem") {
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
          train   = c("TaskRegr", "data.table"),
          predict = c("TaskRegr", "data.table")
        )
      )
    }
  ),

  private = list(
    .train = function(input) {
      task = input[[1L]]
      pv = self$param_set$values
      cut = assert_numeric(pv$cut, null.ok = TRUE, lower = 0)
      max_time = pv$max_time

      res = .discretize_surv_task(task, cut = cut, max_time = max_time, reduction_id = "pem")
      task_pem = res$task
      self$state$cut = res$cut

      # If internal validation task exists, transform it as well
      if (!is.null(task$internal_valid_task)) {
        res_val = .discretize_surv_task(
          task$internal_valid_task,
          cut = self$state$cut,
          max_time = max_time,
          reduction_id = "pem"
        )
        task_pem$internal_valid_task = res_val$task
      }

      list(task_pem, data.table())
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
      # setting time variable to max_time ensures that the ped data spans
      # over all intervals for every subject irrespective of event time
      data[[time_var]] = max_time

      status = data[[event_var]]
      data[[event_var]] = 1

      ped_formula = formulate(sprintf("Surv(%s, %s)", time_var, event_var), ".")
      long_data = pammtools::as_ped(data = data, formula = ped_formula,
                                    cut = cut, max_time = max_time)
      long_data = as.data.table(long_data)
      setnames(long_data, old = "ped_status", new = "pem_status")

      pem_status = id = tend = obs_times = NULL # fixing global binding notes of data.table
      long_data[, pem_status := 0]

      # set correct id
      rows_per_id = nrow(long_data) / length(unique(long_data$id))
      long_data$obs_times = rep(time, each = rows_per_id)
      ids = rep(task$row_ids, each = rows_per_id)
      long_data[, id := ids]

      # set correct pem_status
      reps = long_data[, data.table(count = sum(tend >= obs_times)), by = id]$count
      status = rep(status, times = reps)
      long_data[long_data[, .I[tend >= obs_times], by = id]$V1, pem_status := status]

      # remove some columns from `long_data`
      long_data[, c("tstart", "interval", "obs_times") := NULL]
      task_pem = TaskRegr$new(paste0(task$id, "_pem"), long_data,
                              target = "pem_status")
      task_pem$set_col_roles("id", roles = "original_ids")
      task_pem$set_col_roles("offset", roles = "offset")

      # map observed times back
      reps = table(long_data$id)
      long_data$obs_times = rep(time, each = rows_per_id)

      # subset transformed data
      columns_to_keep = c("id", "obs_times", "tend", "pem_status", "offset")
      long_data = long_data[, columns_to_keep, with = FALSE]

      list(task_pem, long_data)
    }
  )
)

register_pipeop("trafotask_survregr_pem", PipeOpTaskSurvRegrPEM)
