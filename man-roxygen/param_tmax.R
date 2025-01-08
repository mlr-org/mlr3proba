#' @section Parameter details:
#' - `t_max` (`numeric(1)`)\cr
#' Cutoff time \eqn{\tau^*} (i.e. time horizon) to evaluate the measure up to
#' (truncate \eqn{S(t)}).
#' Mutually exclusive with `p_max` or `times`.
#' It's recommended to set `t_max` to avoid division by `eps`, see "Time Cutoff Details" section.
#' If `t_max` is not specified, an `Inf` time horizon is assumed.
#'
