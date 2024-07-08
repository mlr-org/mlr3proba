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
  if (length(types) == 0L) {
    return("-")
  } else {
    return(toString(types))
  }
}

toproper = function(str, split = " ", fixed = TRUE) {
  str = strsplit(str, split, fixed)
  str = lapply(str, function(x) {
    paste0(toupper(substr(x, 1L, 1L)), tolower(substr(x, 2L, 1000)), collapse = split)
  })
  return(unlist(str))
}

check_subsetpattern = function(x, choices, empty.ok = TRUE) { # nolint
  if (all(grepl(paste0(choices, collapse = "|"), x))) {
    return(TRUE)
  } else {
    return(sprintf(
      "Must be a subset of %s, but is %s",
      paste0("{", toString(choices), "}"),
      paste0("{", toString(x), "}")))
  }
}

get_akritas_learner = function() {
  require_namespaces("mlr3extralearners")
  utils::getFromNamespace("LearnerSurvAkritas", "mlr3extralearners")
}

## access private environment of r6 class
r6_private = function(x) {
  x$.__enclos_env__$private
}

## used for plotting
apply_theme = function(theme_object, default_object = NULL) {
  if (getOption("mlr3.theme", TRUE)) theme_object else default_object %??% geom_blank()
}
