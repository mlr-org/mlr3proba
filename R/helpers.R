# used in roxygen templates - adapted from mlr3measures
format_range = function(range) {
  l = min(range)
  u = max(range)

  str = sprintf("%s%s, %s%s",
                if (is.finite(l)) "[" else "(",
                if (is.finite(l)) c(l, l) else c("-\\infty", "-Inf"),
                if (is.finite(u)) c(u, u) else c("\\infty", "Inf"),
                if (is.finite(u)) "]" else ")")
  paste0("\\eqn{", str[1L], "}{", str[2L], "}")
}

# used in roxygen templates
format_types = function(types) {
  paste0(types, collapse = ", ")
}

