#' @section Data used for Estimating Censoring Distribution:
#'
#' If `task` and `train_set` are passed to `$score` then \eqn{G(t)} is fit on training data,
#' otherwise testing data. The first is likely to reduce any bias caused by calculating
#' parts of the measure on the test data it is evaluating. The training data is automatically
#' used in scoring resamplings.
