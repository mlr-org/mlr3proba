#' @section Time Cutoff Details:
#'
#' If `t_max` or `p_max` is given, then the predicted survival function \eqn{S(t)} is
#' truncated at the time cutoff for all observations. This helps mitigate
#' **inflation of the score** which can occur when an observation is censored
#' at the last observed time. In such cases, \eqn{G(t) = 0}, triggering the use
#' of a small constant `eps` instead, see Kvamme et al. (2023).
#' Not using a `t_max` can lead to misleading evaluation, violations of properness
#' and poor optimization outcomes when using this score for model tuning, see
#' Sonabend et al. (2024).
#'
