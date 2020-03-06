# .DenLogspline <- function(x, knots){
#
#   fit = logspline::oldlogspline(x, knots = knots)
#
#   pdf = function(x1){}
#
#   body(pdf) = substitute({
#
#     logspline::doldlogspline(x1,  f)
#
#   }, list (f = fit))
#
#
#   list(distr = distr6::Distribution$new(name = "Penalized Density",
#                                         short_name = "PenalizedDens",
#                                         pdf = pdf))
#
# }
#
#
#

.DensLogspline <- function(dat, test, lbound, ubound, maxknots =0, knots, nknots = 0,
                          penalty = log(length(dat)), silent = TRUE, mind =-1, error.action = 2){

  fit = logspline::logspline(x = dat, maxknots =0, lbound=lbound, ubound=ubound, knots, nknots = nknots,
                             penalty=penalty, silent = silent, mind =mind, error.action = error.action)

  return(logspline::dlogspline(q = test, fit = fit))
}
