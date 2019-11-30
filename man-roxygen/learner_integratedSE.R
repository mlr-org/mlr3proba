#' @description
#' If `integrated == FALSE` then the standard error of the loss, L, is approximated via,
#' \deqn{se(L) = sd(L)/\sqrt{N}}{se(L) = sd(L)/\sqrt N}
#' where \eqn{N} are the number of observations in the test set, and \eqn{sd} is the standard deviation.
#'
#' If `integrated == TRUE` then correlations between time-points need to be taken into account, therefore
#' \deqn{se(L) = \sqrt{\frac{\sum_{i = 1}^M\sum_{j=1}^M \Sigma_{i,j}}{NT^2}}}{se(L) = \sqrt((\sum_{i = 1}^M\sum_{j=1}^M cov(T_i, T_j)) / (NT^2))}
#' where \eqn{\Sigma_{i, j}}{cov(T_i, T_j)} is the sample covariance matrix over \eqn{M} distinct time-points.
