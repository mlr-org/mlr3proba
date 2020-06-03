#' @title Density Task
#'
#' @description
#' This task specializes [Task] for density estimation problems.
#' The target column is assumed to be numeric.
#' The `task_type` is set to `"density"`.
#'
#' Predefined tasks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_tasks].
#'
#' @template param_rows
#' @template param_id
#' @template param_backend
#'
#' @family Task
#' @export
#' @examples
#' task = TaskDens$new("precip", backend = data.frame(target = precip), target = "target")
#' task$task_type
#' task$truth()
TaskDens = R6::R6Class("TaskDens",
  inherit = Task,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param target (`character(1)`)\cr
    #'   Name of the target column.
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

    #' @description
    #' Returns the target column for specified `row_ids`, this is unsupervised so should not be
    #' thought of as a 'true' prediction.
    #' Defaults to all rows with role "use".
    #' @return `numeric()`.
    truth = function(rows = NULL) {
      self$data(rows, cols = self$target_names)[[1]]
    }
  )
)
