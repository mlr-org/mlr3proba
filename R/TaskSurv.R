#' @title Survival Task
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Task]/[TaskSupervised].
#'
#' @description
#' This task specializes [mlr3::Task] and [mlr3::TaskSupervised] for right-censored survival problems.
#' The target column is assumed to be a factor.
#' Predefined tasks are stored in [mlr3::mlr_tasks].
#'
#' The `task_type` is set to `"surv"`.
#'
#' @section Construction:
#' ```
#' t = TaskSurv$new(id, backend, time, status)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Name of the task.
#'
#' * `backend` :: [DataBackend]
#'
#' * `time` :: `numeric()`\cr
#'   Event times.
#'
#' * `status` :: `integer()` | `logical()`\cr
#'   Event indicator. "0"/`FALSE` means alive (no event), "1"/`TRUE` means dead (event).
#'
#' @section Fields:
#' See [mlr3::TaskSupervised].
#'
#' @section Methods:
#' All methods from [mlr3::TaskSupervised], and additionally:
#'
#' * `survfit(strata = character())`\cr
#'   `character()` -> [survival::survfit()]\cr
#'   Creates a [survival::survfit()] object for the survival times.
#'   Argument `strata` can be used to stratify into multiple groups.
#'
#' @family Task
#' @export
#' @examples
#' library(mlr3)
#' lung = mlr3misc::load_dataset("lung", package = "survival")
#' lung$status = (lung$status == 2L)
#' b = as_data_backend(lung)
#' task = TaskSurv$new("lung", backend = b, time = "time", status = "status")
#'
#' task$target_names
#' task$feature_names
#' task$formula()
#' task$truth()
TaskSurv = R6::R6Class("TaskSurv",
                       inherit = TaskSupervised,
                       public = list(
                         initialize = function(id, backend, time, status) {
                           super$initialize(id = id, task_type = "surv", backend = backend, target = c(time, status))

                           status = self$data(cols = status)[[1L]]
                           if (!is.logical(status)) {
                             assert_integerish(status, lower = 0, upper = 1)
                           }
                         },

                         truth = function(row_ids = NULL) {
                           # truth is defined as the survival outcome as a Survival object
                           tn = self$target_names
                           d = self$data(row_ids, cols = self$target_names)
                           Surv(d[[tn[1L]]], as.logical(d[[tn[2L]]]), type = "right")
                         },

                         formula = function(rhs = NULL) {
                           # formula appends the rhs argument to Surv(time, status)~
                           tn = self$target_names
                           lhs = sprintf("Surv(%s, %s)", tn[1L], tn[2L])
                           formulate(lhs, rhs %??% ".", env = getNamespace("survival"))
                         }

                         # survfit is included in mlr3surival but I don't think this should be part
                         #  of the task object. Users can use surv.kaplan or surv.nelson to retrieve this

                         # survfit = function(strata = character()) {
                         #   assert_character(strata, any.missing = FALSE)
                         #   f = self$formula(rhs = strata)
                         #   vars = unique(unlist(extract_vars(f)))
                         #   survfit(f, self$data(cols = vars))
                         # }
                       )
)
