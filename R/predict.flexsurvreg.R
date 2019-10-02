predict.flexsurvreg <- function (object, task, ...)
{

 newdata = task$data(cols = task$feature_names)

 X = stats::model.matrix(formulate(rhs = task$feature_names), data = newdata, xlev = task$levels())[,-1]

 args <- object$aux
 args$knots = as.numeric(args$knots)

 beta = matrix(0)
 if(object$ncovs != 0) beta = matrix(object$res[object$covpars, "est"], nrow = 1)

 pars = matrix(object$res.t[object$dlist$pars, "est"], nrow = nrow(newdata),
               ncol = length(object$dlist$pars), byrow = TRUE)
 X = matrix(X, nrow = nrow(newdata))

  for (j in seq(along = object$dlist$pars)) {
    covinds <- object$mx[[object$dlist$pars[j]]]
    if (length(covinds) > 0)
      pars[, j] <- pars[, j] + beta[, covinds] %*% t(X[, covinds, drop = FALSE])

    pars[, j] <- object$dlist$inv.transforms[[j]](pars[, j])
  }

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
   do.call(fn, c(list(p = x1), args))
 }, list(func = object$dfns$q))

 rand = function(n) {}
 body(rand) = substitute({
   fn = func
   args = as.list(subset(self$parameters()$as.data.table(), select = "value"))$value
   names(args) = unname(unlist(self$parameters()$as.data.table()[,1]))
   do.call(fn, c(list(n = x1), args))
 }, list(func = object$dfns$r))

 params = distr6::ParameterSet$new(id = c(names(args), object$dlist$pars),
                           value = c(list(numeric(length(object$knots)),
                                          "hazard", "log"),rep(list(0), length(object$dlist$pars))),
                           settable = rep(TRUE, length(args)+length(object$dlist$pars)),
                           support = c(list(distr6::Reals$new(dim = length(object$knots)),
                                            distr6::Set$new("hazard","odds","normal"),
                                            distr6::Set$new("log","identity")),
                                       rep(list(distr6::Reals$new()), length(object$dlist$pars)))
 )

  distr = apply(pars, 1, function(y){
    lst = as.list(y)
    names(lst) = object$dlist$pars
    yargs = c(args, lst)

    yparams = params$clone(deep = TRUE)
    yparams$setParameterValue(lst = yargs)

    suppressAll(distr6::Distribution$new(
      short_name = "flexsurv", name = "Flexible Parameteric",
      pdf = pdf, cdf = cdf, quantile = quantile, rand = rand,
      type = distr6::PosReals$new(), support = distr6::PosReals$new(),
      variateForm = "univariate", valueSupport = "continuous",
      parameters = yparams, decorators = c(distr6::CoreStatistics, distr6::ExoticStatistics),
      description = "Royston/Parmar Flexible Parametric Survival Model",
      .suppressChecks = TRUE, suppressMoments = TRUE
    ))

  })

  risk = pars[,1]

  return(list(distr = distr, risk = risk))
}
