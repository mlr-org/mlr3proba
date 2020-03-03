.PDDens <- function(dat, test, base= "gaussian", no.base=41, max.iter=1, lambda0=500, q=3, sort=FALSE,
                    with.border=NULL, m=3, eps=0.01){

  fit <- pendensity::pendensity(form = dat~1, base=base, no.base=no.base, max.iter=max.iter,
                                lambda0 = lambda0, q=q, sort=sort, with.border=with.border, m=m,
                                eps =eps)

  return(pendensity::dpendensity(x = fit, val = test))
}
