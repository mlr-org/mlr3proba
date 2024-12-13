#' @section Time Cutoff Details:
#'
#' If `t_max` or `p_max` is given, then the predicted survival function \eqn{S(t)} is
#' filtered up to the time cutoff for all observations.
#' Also, **observations with observed times** \eqn{t > t_{max}} **are removed**.
#' This is a data processing step to alleviate the problems that arise when using IPCW
#' in cases of administrative censoring, see Kvamme et al. (2023).
#' It also helps alleviate **inflation of the score** in cases where an observation is
#' censored at the last observed time point and no time cutoff is given, which results in
#' \eqn{G(t) = 0} and the use of `eps` instead.
#' The proper version of this score is more affected by this issue, see Sonabend
#' et al. (2024) for more details.
#'
