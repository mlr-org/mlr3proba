.DensGss <- function(dat, test, data = list(), type=NULL,  alpha=1.4, weights=NULL,
                     subset = NULL, na.action=na.omit, id.basis=NULL, nbasis=NULL, seed=NULL,
                     domain=as.list(NULL), quad=NULL, qdsz.depth=NULL, bias=NULL,
                     prec=1e-7, maxiter=30, skip.iter=FALSE){

  fit =   gss::ssden(formula = ~dat, type=type, alpha=alpha, weights=weights, subset = subset,
                     na.action=na.action, id.basis=id.basis, nbasis=nbasis, seed=seed,
                     domain=domain, quad=quad, qdsz.depth=qdsz.depth, bias=bias,
                     prec=prec, maxiter=maxiter, skip.iter=skip.iter)

  return(gss::dssden(object = fit, x = test))

}





