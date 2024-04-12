#' @section Parameter details:
#' - `t_max` (`numeric(1)`)\cr
#' Cutoff time (i.e. time horizon) to evaluate the measure up to.
#' Mutually exclusive with `p_max` or `times`.
#' This will effectively remove test observations for which the time
#' (event or censoring) is less than `t_max`.
#' Useful also for avoiding division by eps which can inflate the score
#' especially in the case when `proper` is `TRUE`.
