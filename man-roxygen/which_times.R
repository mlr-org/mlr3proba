#' @section Time points used for evaluation:
#' If the `times` argument is not specified (`NULL`), then the unique (and
#' sorted) time points from the **test set** are used for evaluation of the
#' time-integrated score.
#' This was a design decision due to the fact that different predicted survival
#' distributions \eqn{S(t)} usually have a **discretized time domain** which may
#' differ, i.e. in the case the survival predictions come from different survival
#' learners.
#' Essentially, using the same set of time points for the calculation of the score
#' minimizes the bias that would come from using different time points.
#' We note that \eqn{S(t)} is by default constantly interpolated for time points that fall
#' outside its discretized time domain.
#'
#' Naturally, if the `times` argument is specified, then exactly these time
#' points are used for evaluation.
#' A warning is given to the user in case some of the specified `times` fall outside
#' of the time point range of the test set.
#' The assumption here is that if the test set is large enough, it should have a
#' time domain/range similar to the one from the train set, and therefore time
#' points outside that domain might lead to interpolation or extrapolation of \eqn{S(t)}.
#'
