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

## constant interpolate CIF matrix to requested `new_times`
interpolate_cif = function(cif_mat, new_times) {
  # predicted time points
  pred_times = as.numeric(colnames(cif_mat))
  if (all(new_times %in% pred_times)) {
    # no interpolation needed
    cif_mat[, as.character(new_times), drop = FALSE]
  } else {
    extend_times = getFromNamespace("C_Vec_WeightedDiscreteCdf", ns = "distr6")
    t(extend_times(new_times, pred_times, cdf = t(cif_mat), lower = TRUE, FALSE))
  }
}

## wrapper around `riskRegression::Score()`
riskRegression_score = function(mat_list, metric, data, formula, times, cause) {
  assert_choice(metric, c("auc", "brier"))

  invoke(
    riskRegression::Score,
    mat_list, # list with one risk prediction matrix (n_obs x times)
    data = data, # (time, event) values for `formula` => n_rows == n_obs
    # `Hist(time, event) ~ 1 => cens.model = 'km') or `Hist(time, event) ~ vars` for 'cox'
    formula = formula,
    summary = base::switch(metric == "brier", "ibs"), # `NULL` otherwise
    se.fit = 0L,
    metrics = metric,
    cens.method = "ipcw",
    cens.model = "km", # "cox" if covariates in formula
    use.event.times = FALSE,
    null.model = FALSE,
    contrasts = FALSE,
    times = times,
    cause = cause
  )
}
