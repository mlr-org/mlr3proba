#' @description
#' If `integrated == FALSE` then the sample mean is taken for the single specified `times`, \eqn{t^*}{t*}, and the returned
#' score is given by
#' \deqn{L(S,t|t^*) = \frac{1}{N} \sum_{i=1}^N L(S_i,t_i|t^*)}{L(S,t|t*) = 1/N \sum_i^N L(S_i,t_i|t*)}
#' where \eqn{N} is the number of observations, \eqn{S_i} is the predicted survival function for
#' individual \eqn{i} and \eqn{t_i} is their true survival time.
#'
#' If `integrated == TRUE` then an approximation to integration is made by taking the mean over all
#' \eqn{T} unique time-points, and then the sample mean over all \eqn{N} observations.
#' \deqn{L(S) = \frac{1}{NT} \sum_{i=1}^N \sum_{j=1}^T L(S_i,t_i|t^*_j)}{L(S) = 1/(NT) \sum_i^N \sum_j^T L(S_i,t_i|t*_j)}
