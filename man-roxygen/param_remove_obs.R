#' @section Parameter details:
#' - `remove_obs` (`logical(1)`)\cr
#' Only effective when `t_max` or `p_max` is provided. Default is `FALSE`.
#' If `TRUE`, then we **remove test observations** for which the observed time (event or censoring) is strictly larger than `t_max`.
#' See "Time Cutoff Details" section for more details.
#'
