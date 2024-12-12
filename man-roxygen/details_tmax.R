#' @section Time Cutoff Details:
#'
#' If `t_max` or `p_max` is given, then the predicted survival function \eqn{S(t)} is
#' filtered up to the time cutoff for all observations.
#' Also, when `proper = TRUE`, \eqn{G(t)} will be filtered up to the cutoff time as well.
#' This helps alleviate inflation of the score in cases where an observation is
#' censored at the last observed time point and no time cutoff is given, which results in
#' \eqn{G(t_{max}) = 0} and the use of `eps` instead.
#'
