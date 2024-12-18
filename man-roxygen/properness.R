#' @section Properness:
#' `r lifecycle::badge("experimental")`
#'
#' <%=proper_id%> is strictly proper when the censoring distribution is independent
#' of the survival distribution and when \eqn{G(t)} is fit on a sufficiently large dataset.
#' <%=improper_id%> is never proper. Use `proper = FALSE` for <%=improper_id%> and
#' `proper = TRUE` for <%=proper_id%>.
#' Results may be very different if many observations are censored at the last
#' observed time due to division by \eqn{1/eps} in `proper = TRUE`.
#'
#' See Sonabend et al. (2024) for more details.
#' The use of `proper = TRUE` is considered experimental and should be used with caution.
#'
