#' @title Assert survival object
#' @description Asserts `x` is a [survival::Surv] object with added checks
#' @param x Object to check
#' @param len If non-NULL checks object is length `len`
#' @param any.missing If `FALSE` then errors if there are any NAs in `x`
#' @param null.ok If `FALSE` then errors if `x` is NULL, otherwise passes
#' @param .var.name Optional variable name to return if assertion fails
#' @noRd
assert_surv = function(x, len = NULL, any.missing = TRUE, null.ok = FALSE, .var.name = vname(x)) { # nolint
  assert_class(x, "Surv", null.ok = null.ok, .var.name = .var.name)
  assert_matrix(x, any.missing = any.missing, nrows = len, null.ok = null.ok, .var.name = .var.name)
}

#' @title Assert survival matrix
#'
#' @description Asserts if the given input matrix is a (discrete) survival
#' probabilities matrix using \CRANpkg{Rcpp}.
#' The following checks are performed:
#'
#' 1. All values are probabilities, i.e. \eqn{S(t) \in [0,1]}
#' 2. Column names correspond to time-points and should therefore be coercable to
#' `numeric` and increasing
#' 3. Per row/observation, the survival probabilities decrease non-strictly, i.e.
#' \eqn{S(t) \ge S(t+1)}
#'
#' @param x (`matrix()`)\cr
#' A matrix of (predicted) survival probabilities.
#' Rows are observations, columns are (increasing) time points.
#'
#' @return if the assertion fails an error occurs, otherwise `NULL` is returned
#' invisibly.
#'
#' @examples
#' x = matrix(data = c(1,0.6,0.4,0.8,0.8,0.7), nrow = 2, ncol = 3, byrow = TRUE)
#' colnames(x) = c(12, 34, 42)
#' x
#'
#' assert_surv_matrix(x)
#'
#' @export
assert_surv_matrix = function(x) {
  times = assert_numeric(as.numeric(colnames(x)), any.missing = FALSE)

  if (is.null(times) || !identical(order(times), seq(ncol(x)))) {
    stop("Survival matrix column names must be increasing numeric (time points)")
  }

  if (!c_assert_surv(x)) {
    stop("Survival probabilities must be (non-strictly) decreasing and between [0, 1]")
  }

  invisible(NULL)
}
