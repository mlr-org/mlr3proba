predict.flexsurvreg <- function (object, task, ...)
{

 # define newdata from the supplied task and convert to model matrix
 newdata = task$data(cols = task$feature_names)
 X = stats::model.matrix(formulate(rhs = task$feature_names), data = newdata, xlev = task$levels())[,-1]

 # collect the auxiliary arguments for the fitted object
 args <- object$aux
 args$knots = as.numeric(args$knots)

 # define matrix of beta coefficients
 beta = matrix(0)
 if(object$ncovs != 0) beta = matrix(object$res[object$covpars, "est"], nrow = 1)

 # collect fitted parameters
 pars = matrix(object$res.t[object$dlist$pars, "est"], nrow = nrow(newdata),
               ncol = length(object$dlist$pars), byrow = TRUE)
 X = matrix(X, nrow = nrow(newdata))

 # for each fitted parameter, multiple by beta coefficients plus offset to return the gamma parameters
  for (j in seq(along = object$dlist$pars)) {
    covinds <- object$mx[[object$dlist$pars[j]]]
    if (length(covinds) > 0)
      pars[, j] <- pars[, j] + beta[, covinds] %*% t(X[, covinds, drop = FALSE])

    pars[, j] <- object$dlist$inv.transforms[[j]](pars[, j])
  }

 # Define the d/p/q/r methods using the d/p/q/r methods that are automatically generated in the fitted
 # object. The parameters referenced are defined below and are based on the gamma parameters above.

 pdf = function(x1) {}
 body(pdf) = substitute({
   fn = func
   args = as.list(subset(self$parameters()$as.data.table(), select = "value"))$value
   names(args) = unname(unlist(self$parameters()$as.data.table()[,1]))
   do.call(fn, c(list(x = x1), args))
 }, list(func = object$dfns$d))

 cdf = function(x1) {}
 body(cdf) = substitute({
   fn = func
   args = as.list(subset(self$parameters()$as.data.table(), select = "value"))$value
   names(args) = unname(unlist(self$parameters()$as.data.table()[,1]))
   do.call(fn, c(list(q = x1), args))
 }, list(func = object$dfns$p))

 quantile = function(p) {}
 body(quantile) = substitute({
   fn = func
   args = as.list(subset(self$parameters()$as.data.table(), select = "value"))$value
   names(args) = unname(unlist(self$parameters()$as.data.table()[,1]))
   do.call(fn, c(list(p = p), args))
 }, list(func = object$dfns$q))

 rand = function(n) {}
 body(rand) = substitute({
   fn = func
   args = as.list(subset(self$parameters()$as.data.table(), select = "value"))$value
   names(args) = unname(unlist(self$parameters()$as.data.table()[,1]))
   do.call(fn, c(list(n = n), args))
 }, list(func = object$dfns$r))

 # The parameter set combines the auxiliary parameters with the fitted gamma coefficients. Whilst the
 # user can set these after fitting, this is generally ill-advised.
 params = distr6::ParameterSet$new(id = c(names(args), object$dlist$pars),
                           value = c(list(numeric(length(object$knots)),
                                          "hazard", "log"),rep(list(0), length(object$dlist$pars))),
                           settable = rep(TRUE, length(args)+length(object$dlist$pars)),
                           support = c(list(distr6::Reals$new(dim = length(object$knots)),
                                            distr6::Set$new("hazard","odds","normal"),
                                            distr6::Set$new("log","identity")),
                                       rep(list(distr6::Reals$new()), length(object$dlist$pars)))
 )

  distr_crank = apply(pars, 1, function(y){
    # Updates the parameters with the fitted values that are extracted above
    lst = as.list(y)
    names(lst) = object$dlist$pars
    yargs = c(args, lst)
    yparams = params$clone(deep = TRUE)
    yparams$setParameterValue(lst = yargs)

    # Defines the distr6 distribution
    distr = suppressAll(distr6::Distribution$new(
      short_name = "flexsurv", name = "Flexible Parameteric",
      pdf = pdf, cdf = cdf, quantile = quantile, rand = rand,
      type = distr6::PosReals$new(), support = distr6::PosReals$new(),
      variateForm = "univariate", valueSupport = "continuous",
      parameters = yparams, decorators = c(distr6::CoreStatistics, distr6::ExoticStatistics),
      description = "Royston/Parmar Flexible Parametric Survival Model",
      .suppressChecks = TRUE, suppressMoments = TRUE
    ))

    # crank is defined as the mean of the predicted survival distribution.
    # this is identical (but more accurate) to the distr$mean integrated method to 2dp
    crank = do.call(object$dfns$mean, yargs)

    return(list(distr, crank))
  })

  return(list(distr = unname(unlist(distr_crank)[seq.int(1, length(distr_crank)*2, 2)]),
         crank = as.numeric(unlist(distr_crank)[seq.int(2, length(distr_crank)*2, 2)])))
}
