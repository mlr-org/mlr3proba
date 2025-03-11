#' @title Convert to a Survival Task
#'
#' @description
#' Convert object to a survival task ([TaskSurv]).
#'
#' @param x (`any`)\cr
#'   Object to convert, e.g. a `data.frame()`.
#' @param ... (`any`)\cr
#'   Additional arguments.
#' @export
as_task_surv = function(x, ...) {
  UseMethod("as_task_surv")
}

#' @rdname as_task_surv
#' @param clone (`logical(1)`)\cr
#'   If `TRUE`, ensures that the returned object is not the same as the input `x`.
#' @export
as_task_surv.TaskSurv = function(x, clone = FALSE, ...) {
  if (clone) x$clone() else x
}

#' @rdname as_task_surv
#' @template param_time
#' @template param_event
#' @template param_time2
#' @template param_type
#' @param id (`character(1)`)\cr
#'   Id for the new task.
#'   Defaults to the (deparsed and substituted) name of `x`.
#' @export
as_task_surv.data.frame = function(x, time = "time", event = "event", time2 = "time2",
                                   type = "right", id = deparse(substitute(x)), ...) {
  assert_data_frame(x, min.rows = 1L, min.cols = 1L, col.names = "unique")

  ii = which(map_lgl(keep(x, is.double), anyInfinite))
  if (length(ii)) {
    warningf("Detected columns with unsupported Inf values in data: %s", str_collapse(names(ii)))
  }

  TaskSurv$new(id = id, backend = x, time = time, event = event, time2 = time2, type = type)
}

#' @rdname as_task_surv
#' @export
as_task_surv.DataBackend = function(x, time = "time", event = "event", time2, type = "right", id = deparse(substitute(x)), ...) {
  TaskSurv$new(id = id, backend = x, time = time, event = event, time2 = time2, type = type)
}
