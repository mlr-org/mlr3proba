#' @section Parameter details:
#' - `proper` (`logical(1)`)\cr
#'  If `TRUE` then weights scores by the censoring distribution at
#'  the observed event time, which results in a strictly proper scoring
#'  rule if censoring and survival time distributions are independent
#'  and a sufficiently large dataset is used, see Sonabend et al. (2024).
#'  If `FALSE` then weights scores by the Graf method which is the
#'  more common usage but the loss is not proper.
#'  See "Properness" section for more details.
#'
