#' @description
#' The `dist` parameter is specified slightly differently than in [mboost]. Whereas the latter
#' takes in objects, in this learner instead a string is specified in order to identify which distribution
#' to use. As the default in [mboost] is the Gaussian family, which is not compatible with
#' survival models, instead we have by default `"coxph"`.
#'
#' If the value given to the \code{Family} parameter is "custom.family" then an object of class
#' [mboost::Family()] needs to be passed to the \code{custom.family} parameter.
