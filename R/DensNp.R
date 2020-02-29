.DensNp <- function(tdat,  bws,
                    ckertype = "gaussian",
                    ckeorder = 2,
                    bwmethod = "cv.ml"){

  pdf <- function(x1){}

  body(pdf) <- substitute({

  if(missing(bws)) {

    return(np::npudens(bws =np::npudensbw(dat = tdat, edat = x1,   ckertype = ckertype, ckeorder =ckeorder,
                                            bwmethod=bwmethod), tdat = tdat, edat = x1, ckertype = ckertype,
                                            ckeorder =ckeorder)$dens)

  } else{

     return(np::npudens(bws = np::npudensbw(dat = tdat, edat = x1,   bws = bws,
                                           bandwidth.compute = FALSE), tdat = tdat, edat = x1,
                                          ckertype = ckertype, ckeorder =ckeorder)$dens)
  }

  })

  list(distr = distr6::Distribution$new(name = "Penalized Density",
                                        short_name = "PenalizedDens",
                                        pdf = pdf))


}

