#' @section Parameter details:
#' - `t_max` (`numeric(1)`)\cr
#' Cutoff time \eqn{\tau^*} (i.e. time horizon) to evaluate the measure up to.
#' Mutually exclusive with `p_max` or `times`.
#' This will effectively remove test observations for which the observed time
#' (event or censoring) is strictly more than `t_max` in the case of `proper = TRUE`.
#' It's recommended to set `t_max` to avoid division by `eps`, see "Time Cutoff Details" section.
#' If `t_max` is not specified, an `Inf` time horizon is assumed.
