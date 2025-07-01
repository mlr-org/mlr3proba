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
