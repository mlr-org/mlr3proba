#' @title Convert to a Competing Risks Task
#'
#' @description
#' Convert object to a competing risks task ([TaskCompRisks]).
#'
#' @param x (`any`)\cr
#'   Object to convert, e.g. a `data.frame()`.
#' @param ... (`any`)\cr
#'   Additional arguments.
#' @export
as_task_cmprsk = function(x, ...) {
  UseMethod("as_task_cmprsk")
}

#' @rdname as_task_cmprsk
#' @param clone (`logical(1)`)\cr
#'   If `TRUE`, ensures that the returned object is not the same as the input `x`.
#' @export
as_task_cmprsk.TaskCompRisks = function(x, clone = FALSE, ...) {
  if (clone) x$clone() else x
}

#' @rdname as_task_cmprsk
#' @template param_time
#' @template param_event
#' @param id (`character(1)`)\cr
#'   Id for the new task.
#'   Defaults to the (deparsed and substituted) name of `x`.
#' @export
as_task_cmprsk.data.frame = function(x, time = "time", event = "event",
                                      id = deparse(substitute(x)), ...) {
  assert_data_frame(x, min.rows = 1L, min.cols = 1L, col.names = "unique")

  ii = which(map_lgl(keep(x, is.double), anyInfinite))
  if (length(ii)) {
    warningf("Detected columns with unsupported Inf values in data: %s", str_collapse(names(ii)))
  }

  TaskCompRisks$new(id = id, backend = x, time = time, event = event, ...)
}

#' @rdname as_task_cmprsk
#' @export
as_task_cmprsk.DataBackend = function(x, time = "time", event = "event",
                                       id = deparse(substitute(x)), ...) {
  TaskCompRisks$new(id = id, backend = x, time = time, event = event, ...)
}
