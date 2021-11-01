#' @title Convert to a Density Task
#'
#' @description
#' Convert object to a density task ([TaskDens]).
#'
#' @param x (`any`)\cr
#'   Object to convert, e.g. a `data.frame()`.
#' @param ... (`any`)\cr
#'   Additional arguments.
#' @export
as_task_dens = function(x, ...) {
  UseMethod("as_task_dens")
}


#' @rdname as_task_dens
#' @param clone (`logical(1)`)\cr
#'   If `TRUE`, ensures that the returned object is not the same as the input `x`.
#' @export
as_task_dens.TaskDens = function(x, clone = FALSE, ...) { # nolint
  if (clone) x$clone() else x
}


#' @rdname as_task_dens
#' @param id (`character(1)`)\cr
#'   Id for the new task.
#'   Defaults to the (deparsed and substituted) name of `x`.
#' @export
as_task_dens.data.frame = function(x, id = deparse(substitute(x)), ...) { # nolint
  TaskDens$new(id = id, backend = x)
}


#' @rdname as_task_dens
#' @export
as_task_dens.DataBackend = function(x, id = deparse(substitute(x)), ...) { # nolint
  TaskDens$new(id = id, backend = x)
}
