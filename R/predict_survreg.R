predict_survreg = function(object, task, type = "aft"){

  # Extracts baseline distribution and the model fit, performs assertions
  basedist = object$basedist
  fit = object$fit
  distr6::assertDistribution(basedist)
  assertClass(fit, "survreg")

  # define newdata from the supplied task and convert to model matrix
  newdata = task$data(cols = task$feature_names)
  x = stats::model.matrix(formulate(rhs = task$feature_names), data = newdata, xlev = task$levels())[,-1]

  # linear predictor defined by the fitted cofficients multiplied by the model matrix (i.e. covariates)
  lp = matrix(fit$coefficients[-1], nrow = 1) %*% t(x)

  # checks and parameterises the chosen model type: proportional hazard (ph), accelerated failure time (aft), odds.
  # PH: h(t) = h0(t)exp(lp)
  # AFT: h(t) = exp(-lp)h0(t/exp(lp))
  # PO: h(t)/h0(t) = {1 + (exp(lp)-1)S0(t)}^-1

  dist = distr6::toproper(fit$dist)

  if (type == "ph") {
    name = paste(dist, "Proportional Hazards Model")
    short_name = paste0(dist,"PH")
    description = paste(dist, "Proportional Hazards Model with negative log-likelihood", -fit$loglik[2])
  } else if (type == "aft") {
    name = paste(dist, "Accelerated Failure Time Model")
    short_name = paste0(dist,"AFT")
    description = paste(dist, "Accelerated Failure Time Model with negative log-likelihood", -fit$loglik[2])
  } else if (type == "po") {
    name = paste(dist, "Proportional Odds Model")
    short_name = paste0(dist,"PO")
    description = paste(dist, "Proportional Odds Model with negative log-likelihood", -fit$loglik[2])
  }

  params = list(list(name = name,
                     short_name = short_name,
                     type = distr6::PosReals$new(),
                     support = distr6::PosReals$new(),
                     valueSupport = "continuous",
                     variateForm = "univariate",
                     description = description,
                     .suppressChecks = TRUE,
                     suppressMoments = TRUE,
                     pdf = function(){},
                     cdf = function(){}
                     ))

  params = rep(params, length(lp))

  cdf = function(x1){}
  pdf = function(x1) {}

  if (type == "ph") {
    for(i in 1:length(lp)){
      body(cdf) = substitute(1 - (basedist$survival(x1)^exp(x)), list(x = lp[i]))
      body(pdf) = substitute((exp(x) * basedist$hazard(x1)) * (1 - self$cdf(x1)), list(x = lp[i]))
      params[[i]]$pdf = pdf
      params[[i]]$cdf = cdf
    }
  } else if (type == "aft"){
    for(i in 1:length(lp)){
      body(cdf) = substitute(1 - (basedist$survival(x1/exp(x))), list(x = lp[i]))
      body(pdf) = substitute((exp(-x) * basedist$hazard(x1/exp(x))) * (1 - self$cdf(x1)), list(x = lp[i]))
      params[[i]]$pdf = pdf
      params[[i]]$cdf = cdf
    }
  } else if (type == "po"){
    for(i in 1:length(lp)){
      body(cdf) = substitute(1 - (basedist$survival(x1) * (exp(-x) + (1-exp(-x))*basedist$survival(x1))^-1), list(x = lp[i]))
      body(pdf) = substitute((basedist$hazard(x1) * (1 - ( basedist$survival(x1) / ( ((exp(x)-1)^-1) + basedist$survival(x1))))) *
                               (1 - self$cdf(x1)), list(x = lp[i]))
      params[[i]]$pdf = pdf
      params[[i]]$cdf = cdf
    }
  }


  distr = distr6::VectorDistribution$new(distribution = "Distribution", params = params,
                                         decorators = c("CoreStatistics","ExoticStatistics"))

  return(list(lp = as.numeric(lp), distr = distr))
}
