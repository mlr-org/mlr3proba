#' @section Data used for Estimating Censoring Distribution:
#'
#' If `task` and `train_set` are passed to `$score` then \eqn{G(t)} is fit using
#' **all observations** from the train set, otherwise the test set is used.
#' Using the train set is likely to reduce any bias caused by calculating parts of the
#' measure on the test data it is evaluating.
#' Also usually it means that more data is used for fitting the censoring
#' distribution \eqn{G(t)} via the Kaplan-Meier.
#' The training data is automatically used in scoring resamplings.
#'
