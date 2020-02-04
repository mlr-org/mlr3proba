predict_flexsurvreg <- function (object, task, ...)
{

   # define newdata from the supplied task and convert to model matrix
   newdata = task$data(cols = task$feature_names)
   X = stats::model.matrix(formulate(rhs = task$feature_names), data = newdata, xlev = task$levels())

   # collect the auxiliary arguments for the fitted object
   args <- object$aux
   args$knots <- as.numeric(args$knots)

   # define matrix of coeffs coefficients
   coeffs = matrix(object$coefficients[c("gamma0", colnames(X)[-1])], nrow = 1)

   # collect fitted parameters
   pars = matrix(object$res.t[object$dlist$pars, "est"], nrow = nrow(newdata),
                 ncol = length(object$dlist$pars), byrow = TRUE)
   colnames(pars) = object$dlist$pars

   # calculate the linear predictor as X\beta, note intercept not included in model.matrix
   # so added manually
   pars[, "gamma0"] <- coeffs %*% t(X)

   # if any inverse transformations exist then apply them
   invs = sapply(object$dlist$inv.transforms, function(tr) body(tr) != "x")
   if(any(invs)){
      for(i in which(invs)) {
         pars[, i] <- object$dlist$inv.transforms[[i]](pars[, i])
      }
   }

   # once inverse transformed we can collect the linear predictor
   lp = pars[, "gamma0"]

   # Define the d/p/q/r methods using the d/p/q/r methods that are automatically generated in the fitted
   # object. The parameters referenced are defined below and are based on the gamma parameters above.
   pdf = function(x1) {}
   body(pdf) = substitute({
      fn = func
      args = as.list(subset(as.data.table(self$parameters()), select = "value"))$value
      names(args) = unname(unlist(as.data.table(self$parameters())[,1]))
      do.call(fn, c(list(x = x1), args))
   }, list(func = object$dfns$d))

   cdf = function(x1) {}
   body(cdf) = substitute({
      fn = func
      args = as.list(subset(as.data.table(self$parameters()), select = "value"))$value
      names(args) = unname(unlist(as.data.table(self$parameters())[,1]))
      do.call(fn, c(list(q = x1), args))
   }, list(func = object$dfns$p))

   quantile = function(p) {}
   body(quantile) = substitute({
      fn = func
      args = as.list(subset(as.data.table(self$parameters()), select = "value"))$value
      names(args) = unname(unlist(as.data.table(self$parameters())[,1]))
      do.call(fn, c(list(p = p), args))
   }, list(func = object$dfns$q))

   rand = function(n) {}
   body(rand) = substitute({
      fn = func
      args = as.list(subset(as.data.table(self$parameters()), select = "value"))$value
      names(args) = unname(unlist(as.data.table(self$parameters())[,1]))
      do.call(fn, c(list(n = n), args))
   }, list(func = object$dfns$r))

   # The parameter set combines the auxiliary parameters with the fitted gamma coefficients. Whilst the
   # user can set these after fitting, this is generally ill-advised.
   parameters = distr6::ParameterSet$new(id = c(names(args), object$dlist$pars),
                                         value = c(list(numeric(length(object$knots)),
                                                        "hazard", "log"),rep(list(0), length(object$dlist$pars))),
                                         settable = rep(TRUE, length(args)+length(object$dlist$pars)),
                                         support = c(list(set6::Reals$new()^length(object$knots)),
                                                          set6::Set$new("hazard","odds","normal"),
                                                          set6::Set$new("log","identity"),
                                                     rep(list(set6::Reals$new()), length(object$dlist$pars)))
   )

   pars = data.table::data.table(t(pars))
   pargs = data.table::data.table(matrix(args, ncol = ncol(pars), nrow = length(args)))
   pars = rbind(pars, pargs)

   params = lapply(pars, function(x){
      x = as.list(x)
      names(x) = c(object$dlist$pars, names(args))
      yparams = parameters$clone(deep = TRUE)
      ind = match(yparams$.__enclos_env__$private$.parameters$id, names(x))
      yparams$.__enclos_env__$private$.parameters$value = x[ind]

      yparams
   })

   params = lapply(params, function(x) list(parameters = x))

   shared_params = list(name = "Flexible Parameteric",
                        short_name = "Flexsurv",
                        type = set6::PosReals$new(),
                        support = set6::PosReals$new(),
                        valueSupport = "continuous",
                        variateForm = "univariate",
                        description = "Royston/Parmar Flexible Parametric Survival Model",
                        .suppressChecks = TRUE,
                        suppressMoments = TRUE,
                        pdf = pdf, cdf = cdf, quantile = quantile, rand = rand
   )

   distr = distr6::VectorDistribution$new(distribution = "Distribution", params = params,
                                          shared_params = shared_params, decorators = c("CoreStatistics","ExoticStatistics"))

   return(list(distr = distr, lp = lp))
}
