#' @section Time Cutoff Details:
#'
#' If `t_max` or `p_max` is given, then \eqn{G(t)} will be fitted using **all observations** from the
#' train set (or test set) and only then the cutoff time will be applied.
#' This is to ensure that more data is used for fitting the censoring distribution via the
#' Kaplan-Meier.
#' Setting the `t_max` can help alleviate inflation of the score when `proper` is `TRUE`,
#' in cases where an observation is censored at the last observed time point.
#' This results in \eqn{G(t_{max}) = 0} and the use of `eps` instead (when `t_max` is `NULL`).
