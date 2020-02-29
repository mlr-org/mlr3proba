#' @title Density Task
#'
#' @usage NULL
#' @format [R6::R6Class] object inheriting from [Task].
#'
#' @description
#' This task specializes [Task] for density estimation problems.
#' The target column is assumed to be numeric.
#' The `task_type` is set to `"density"`.
#'
#' Predefined tasks are stored in the [mlr3misc::Dictionary] [mlr_tasks].
#'
#' @section Construction:
#' ```
#' t = TaskDens$new(id, backend, target)
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier for the task.
#'
#' * `backend` :: ([DataBackend] | `data.frame()` | ...)\cr
#'   Either a [DataBackend], or any object which is convertible to a DataBackend with `as_data_backend()`.
#'   E.g., a `data.frame()` will be converted to a [DataBackendDataTable].
#'
#' * `target` :: `character(1)`\cr
#'   Name of the target column.
#'
#' @section Fields:
#' See [Task].
#'
#' @section Methods:
#' All methods from [Task], and additionally:
#'
#' * `truth(rows = NULL)` :: `any`\cr
#'   True response for specified `row_ids`. Format depends on the task type.
#'   Defaults to all rows with role "use".
#'
#' @family Task
#' @export
#' @examples
#' task = TaskDens$new("precip", backend = data.frame(target = precip), target = "target")
#' task$task_type
#' task$truth()
TaskDens <- R6::R6Class("TaskDens", inherit = Task,
  public = list(
    initialize = function(id, backend, target) {
      super$initialize(id = id, task_type = "dens", backend = backend)
      assert_subset(target, self$col_roles$feature)
      self$col_roles$target = target
      self$col_roles$feature = setdiff(self$col_roles$feature, target)


      type = subset(self$col_info, id == target, "type")
      if (!(type %in% c("integer", "numeric"))) {
        stop("Target column '%s' must be numeric", target)
      }
    },
    truth = function(rows = NULL) {
      self$data(rows, cols = self$target_names)[[1]]
    }
  )
)
