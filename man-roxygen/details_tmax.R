#' @details 
#' If `t_max` or `p_max` is given, then \eqn{G(t)} will be fitted using **all observations** from the
#' train set (or test set) and only then the cutoff time will be applied.
#' This is to ensure that more data is used for fitting the censoring distribution via the
#' Kaplan-Meier and allow the user to remove the latest time points that may introduce 
#' inflation of the proper score due to division by `eps`.
