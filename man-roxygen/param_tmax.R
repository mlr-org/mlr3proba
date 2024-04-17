#' @section Parameter details:
#' - `t_max` (`numeric(1)`)\cr
#' Cutoff time (i.e. time horizon) to evaluate the measure up to.
#' Mutually exclusive with `p_max` or `times`.
#' This will effectively remove test observations for which the time
#' (event or censoring) is less than `t_max`.
#' It's recommended to set `t_max` to avoid division by `eps`, see Details.
