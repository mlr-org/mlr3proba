#' @title Assert survival object
#' @description Asserts `x` is a [survival::Surv] object with added checks
#' @param x Object to check
#' @param len If non-NULL checks object is length `len`
#' @param any.missing If `FALSE` then errors if there are any NAs in `x`
#' @param null.ok If `FALSE` then errors if `x` is NULL, otherwise passes
#' @param .var.name Optional variable name to return if assertion fails
#' @export
assert_surv = function(x, len = NULL, any.missing = TRUE, null.ok = FALSE, .var.name = vname(x)) { # nolint
  assert_class(x, "Surv", null.ok = null.ok, .var.name = .var.name)
  assert_matrix(x, any.missing = any.missing, nrows = len, null.ok = null.ok, .var.name = .var.name)
}
