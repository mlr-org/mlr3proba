#' @title Density Task
#'
#' @description
#' This task specializes [TaskUnsupervised] for density estimation problems.
#' The data in `backend` should be a numeric vector or a one column matrix-like object.
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
#' task = TaskDens$new("precip", backend = precip)
#' task$task_type
TaskDens = R6::R6Class("TaskDens",
  inherit = TaskUnsupervised,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param backend ([DataBackend])\cr
    #' Either a [DataBackend], a matrix-like object, or a numeric vector.
    #' If weights are used then two columns expected, otherwise one column. The weight column
    #' must be clearly specified (via `[Task]$col_roles`) or the learners will break.
    initialize = function(id, backend) {

      if (test_numeric(backend)) {
        backend = data.frame(x = backend)
      } else if (test_class(backend, "DataBackend")) {
        assert_numeric(backend$ncol, lower = 2, upper = 3)
      } else {
        assert_numeric(ncol(backend), lower = 1, upper = 2)
      }

      super$initialize(id = id, task_type = "dens", backend = backend)
    }
  )
)
