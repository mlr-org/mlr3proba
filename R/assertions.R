assert_surv = function(x, len = NULL, any.missing = TRUE, null.ok = FALSE, .var.name = vname(x)) { # nolint
  assert_class(x, "Surv", null.ok = null.ok, .var.name = .var.name)
  assert_matrix(x, any.missing = any.missing, nrows = len, null.ok = null.ok, .var.name = .var.name)
}
