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
  if (length(types) == 0) {
    return("-")
  } else {
    return(paste0(types, collapse = ", "))
  }
}

toproper = function(str, split = " ", fixed = TRUE) {
  str = strsplit(str, split, fixed)
  str = lapply(str, function(x) {
    paste0(toupper(substr(x, 1, 1)), tolower(substr(x, 2, 1000)), collapse = split)
  })
  return(unlist(str))
}

check_subsetpattern = function(x, choices, empty.ok = TRUE) { # nolint
  if (all(grepl(paste0(choices, collapse = "|"), x))) {
    return(TRUE)
  } else {
    return(sprintf(
      "Must be a subset of %s, but is %s",
      paste0("{", paste0(choices, collapse = ", "), "}"),
      paste0("{", paste0(x, collapse = ", "), "}")))
  }
}
