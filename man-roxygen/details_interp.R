#' @section Interpolation:
#'
#' To evaluate scores involving subject-specific survival functions
#' \eqn{S_i(t)}, we perform **linear interpolation** on the discrete survival
#' values provided in the prediction.
#' Duplicate survival values are removed prior to interpolation to ensure strict
#' monotonicity and non-negative density values.
#' Therefore we are left with the distinct survival time points
#' \eqn{t_0 < \cdots < t_k} and the corresponding survival values \eqn{S(t_j)}.
#'
#' Interpolation is performed using base R’s `approx()` with `method = "linear"`
#' and `rule = 2`, ensuring:
#'
#' - **Left extrapolation** (for \eqn{t < t_0}) assumes \eqn{S(0) = 1} and uses
#' the slope from \eqn{(0, 1)} to \eqn{(t_0, S(t_0))}.
#' - **Right extrapolation** (for \eqn{t > t_k}) uses the slope from the last
#' interval \eqn{(t_{k-1}, S(t_{k-1}))} to \eqn{(t_k, S(t_k))}, with results
#' truncated at 0 to preserve non-negativity.
#'
#' This ensures a continuous, piecewise-linear survival function \eqn{S(t)} that
#' satisfies \eqn{S(0) = 1} and remains non-increasing and non-negative across
#' the entire domain.
#'
#' The density is estimated at \eqn{t_i} as follows:
#'
#' \deqn{
#' f_i(t_i) = -\frac{S_i(t_{i+1}) - S_i(t_i)}{t_{i+1} - t_i}
#' }
#'
#' This corresponds to the (negative) slop of the \eqn{S_i(t)} between the closest
#' grid point after \eqn{t_i} and \eqn{t_i} itself.
#'
