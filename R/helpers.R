# General helper functions

# used in roxygen templates - adapted from mlr3measures
format_range = function(range) {
  l = min(range)
  u = max(range)

  str = sprintf(
    "%s%s, %s%s",
    if (is.finite(l)) "[" else "(",
    if (is.finite(l)) c(l, l) else c("-\\infty", "-Inf"),
    if (is.finite(u)) c(u, u) else c("\\infty", "Inf"),
    if (is.finite(u)) "]" else ")")
  paste0("\\eqn{", str[1L], "}{", str[2L], "}")
}

# used in roxygen templates
format_types = function(types) {
  if (length(types) == 0L) "-" else toString(types)
}

## from `mlr3extralearners`
ordered_features = function(task, learner) {
  # the data_prototype is not present when calling the workhorse function,
  # as it can blow up memory usage
  cols = names(learner$state$data_prototype) %??% learner$state$feature_names
  task$data(cols = intersect(cols, task$feature_names))
}

## create GraphLearner
create_grlrn = function(gr, graph_learner = FALSE) {
  if (graph_learner) {
    gr = as_learner(gr)
  }

  gr
}

#' Discretize a survival task using pammtools::as_ped
#'
#' @param task [TaskSurv] survival task
#' @param cut [numeric] vector of cut points or integer indicating number of intervals
#' @param max_time [numeric] maximum time horizon to extend the data (optional)
#' @param reduction_id [character] either "disc" or "pem"
#'
#' @return list with:
#'   - `task_{disc|pem}`: the new `TaskClassif` (DiscreteTime) or `TaskRegr` (PEM)
#'   - `cut`: cut points used for time discretization
#' @noRd
.discretize_surv_task = function(task, cut = NULL, max_time = NULL, reduction_id = "disc") {
  assert_choice(reduction_id, c("disc", "pem"))
  assert_true(task$cens_type == "right")
  data = task$data()

  col_status = paste0(reduction_id, "_status")
  if (col_status %in% names(data)) {
    stopf("'%s\' cannot be a column in the input data.", col_status)
  }

  time_var = task$target_names[1]
  event_var = task$target_names[2]

  # Handle integer cut as number of intervals
  if (test_int(cut, lower = 1)) {
    cut = seq(0, data[get(event_var) == 1, max(get(time_var))], length.out = cut + 1)
  }

  if (!is.null(max_time)) {
    assert(max_time > data[get(event_var) == 1, min(get(time_var))],
           .var.name = "max_time must be greater than the minimum event time.")
  }

  form = formulate(sprintf("Surv(%s, %s)", time_var, event_var), ".")

  long_data = as.data.table(
    pammtools::as_ped(data = data, formula = form, cut = cut, max_time = max_time)
  )
  setnames(long_data, old = "ped_status", new = col_status)

  # Format outcome for discrete time
  if (reduction_id == "disc") {
    long_data[[col_status]] = factor(long_data[[col_status]], levels = c("0", "1"))
  }
  long_data$disc_status = factor(long_data$disc_status, levels = c("0", "1"))

  # Remove internal columns
  cols_to_remove = c("tstart", "interval")
  if (reduction_id == "disc") {
    cols_to_remove = c(cols_to_remove, "offset")
  }
  long_data[, (cols_to_remove) := NULL]

  # Map row ids
  reps = table(long_data$id)
  ids = rep(task$row_ids, times = reps)
  id = NULL # silence data.table note
  long_data[, id := ids]

  # Build task
  task_id = paste0(task$id, "_", reduction_id)
  if (reduction_id == "disc") {
    new_task = TaskClassif$new(task_id, long_data, target = col_status, positive = "1")
  } else if (reduction_id == "pem") {
    new_task = TaskRegr$new(task_id, long_data, target = col_status)
    new_task$set_col_roles("offset", roles = "offset")
  }
  new_task$set_col_roles("id", roles = "original_ids")

  list(
    task = new_task,
    cut = attributes(long_data)$trafo_args$cut
  )
}

