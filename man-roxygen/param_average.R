#' @param average (`character(1)`)\cr
#'   How to average multiple [Prediction][mlr3::Prediction]s from a
#'   [ResampleResult][mlr3::ResampleResult].
#'
#'   The default, `"macro"`, calculates the individual performances scores for
#'   each [Prediction][mlr3::Prediction] and then uses the function defined in
#'   `$aggregator` to average them to a single number.
#'
#'   If set to `"micro"`, the individual [Prediction][mlr3::Prediction] objects
#'   are first combined into a single new [Prediction][mlr3::Prediction] object
#'   which is then used to assess the performance.
#'   The function in `$aggregator` is not used in this case.
