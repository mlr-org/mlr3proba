#' @section Parameter details:
#' - `method` (`integer(1)`)\cr
#' If `integrate == TRUE`, this selects the integration weighting method.
#' `method == 1` corresponds to weighting each time-point equally
#' and taking the mean score over discrete time-points.
#' `method == 2` corresponds to calculating a mean weighted by the
#' difference between time-points.
#' `method == 2` is the default value, to be in line with other packages.
