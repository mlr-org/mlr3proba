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

#' @title Assert survival matrix
#'
#' @description Asserts if the given input matrix is a (discrete) survival
#' probabilities matrix using [Rcpp] code.
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

#' @title Assert CIF list
#'
#' @description Asserts if the given input list is a list of Cumulative Incidence
#' matrices.
#'
#' @param x (`list()`)\cr
#' A list of CIF matrices, each one with dimensions (observations x times).
#' @param n_rows (`numeric(1)`)\cr
#' Expected number of rows of each matrix element of the input list.
#' @param n_cmp_events (`numeric(1)`)\cr
#' Expected number of competing events which is the number of elements in input
#' CIF list.
#'
#' @return if the assertion fails an error occurs, otherwise `NULL` is returned
#' invisibly.
#'
#' @export
assert_cif_list = function(x, n_rows = NULL, n_cmp_events = NULL) {
  # List of matrices, with at least 2 elements/competing risks
  assert_list(x, types = "matrix", any.missing = FALSE, min.len = 2,
              len = n_cmp_events, names = "named")
  for (mat in x) {
    # Each element a matrix
    assert_matrix(mat, any.missing = FALSE, min.rows = 1, min.cols = 1, col.names = "named")
    # check `nrow` == `n_obs`
    if (!is.null(n_rows)) {
      assert_true(nrow(mat) == n_rows, .var.name = sprintf("CIF matrix has %i rows and not %i (number of observations)", nrow(mat), n_rows))
    }
    # check column names => time points
    assert_numeric(as.numeric(colnames(mat)), lower = 0, unique = TRUE, sorted = TRUE,
                   any.missing = FALSE, null.ok = FALSE, .var.name = "Colnames must be coersable to positive, unique, increasing numeric time points")
  }

  invisible(NULL)
}
