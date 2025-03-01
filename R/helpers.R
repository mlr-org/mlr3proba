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

check_subsetpattern = function(x, choices, empty.ok = TRUE) { # nolint
  if (all(grepl(paste0(choices, collapse = "|"), x))) {
    TRUE
  } else {
    sprintf(
      "Must be a subset of %s, but is %s",
      paste0("{", toString(choices), "}"),
      paste0("{", toString(x), "}"))
  }
}

get_akritas_learner = function() {
  require_namespaces("mlr3extralearners")
  utils::getFromNamespace("LearnerSurvAkritas", "mlr3extralearners")
}

## used for plotting
apply_theme = function(theme_object, default_object = NULL) {
  if (getOption("mlr3.theme", TRUE)) theme_object else default_object %??% geom_blank()
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
