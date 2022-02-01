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
as_task_surv.TaskSurv = function(x, clone = FALSE, ...) { # nolint
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
as_task_surv.data.frame = function(x, time = "time", event = "event", time2, type = "right", id = deparse(substitute(x)), ...) { # nolint
  ii = which(map_lgl(keep(x, is.double), anyInfinite))
  if (length(ii)) {
    warningf("Detected columns with unsupported Inf values in data: %s", str_collapse(names(ii)))
  }

  TaskSurv$new(id = id, backend = x, time = time, event = event, time2 = time2, type = type)
}


#' @rdname as_task_surv
#' @export
as_task_surv.DataBackend = function(x, time = "time", event = "event", time2, type = "right", id = deparse(substitute(x)), ...) { # nolint
  TaskSurv$new(id = id, backend = x, time = time, event = event, time2 = time2, type = type)
}

#' @rdname as_task_surv
#' @export
as_task_surv.formula = function(x, data, id = deparse(substitute(data)), ...) { # nolint
  all_vars = all.vars(x)
  assert_subset(all_vars, c(names(data), "."), .var.name = "formula")
  if (!attributes(terms(x, data = data))$response) {
    stopf("Formula %s is missing a response", format(x))
  }

  tab = model.frame(x, data)
  surv = stats::model.response(tab)
  tab = cbind(
    data.table(.__time__ = surv[, 1L], .__status__ = surv[, 2L]),
    tab[, -1L, drop = FALSE]
  )
  setnames(tab, c(".__time__", ".__status__"), all_vars[1:2])

  as_task_surv(tab, time = all_vars[1L], event = all_vars[2L], id = id)
}
