#' @description
#' If `integrated == FALSE` then the sample mean is taken for the single specified `times`, \eqn{t}, and the returned
#' score is given by
#' \deqn{L(S, t*) = \frac{1}{N} \sum_i^N L(S_i, t*)}{L(S, t*) = 1/N \sum_i^N L(S_i, t*)}
#' where \eqn{N} is the number of observations and \eqn{S_i} is the predicted survival function for
#' individual \eqn{i}.
#'
#' If `integrated == TRUE` then an approximation to integration is made by taking the mean over all unique
#' time-points, \eqn{T}, and then the sample mean over all \eqn{N} observations.
#' \deqn{L(S) = \frac{1}{NT} \sum_i^N \sum_j^T L(S_i, t_j)}{L(S) = 1/(NT) \sum_i^N \sum_j^T L(S_i, t_j)}
