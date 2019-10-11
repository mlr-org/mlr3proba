predict_survreg = function(object, task, type = "aft", predict_type = "all"){

  # Extracts baseline distribution and the model fit, performs assertions
  basedist = object$basedist
  fit = object$fit
  distr6::testDistribution(basedist)
  assertClass(fit, "survreg")

  # define newdata from the supplied task and convert to model matrix
  newdata = task$data(cols = task$feature_names)
  x = stats::model.matrix(formulate(rhs = task$feature_names), data = newdata, xlev = task$levels())[,-1]

  # linear predictor defined by the fitted cofficients multiplied by the model matrix (i.e. covariates)
  lp = matrix(fit$coefficients[-1], nrow = 1) %*% t(x)

  # checks and parameterises the chosen model type: proportional hazard (ph), accelerated failure time (aft), odds.
  # PH: h(t) = h0(t)exp(lp)
  # AFT: h(t) = exp(-lp)h0(t/exp(lp))
  # PO: h(t)/h0(t) = {1 + (exp(lp)-1)S-(t)}^-1
  if(type == "ph"){
    distr = lapply(lp, function(x){
      haz = function(x1) exp(x) * basedist$hazard(x1)
      cdf = function(x1) 1 - (basedist$survival(x1)^exp(x))
      pdf = function(x1) haz(x1) * (1 - cdf(x1))
      distr6::Distribution$new(name = paste(fit$dist, "Proportional Hazards Model"),
                               short_name = paste0(fit$dist,"PH"),
                               type = distr6::PosReals$new(), support = distr6::PosReals$new(),
                               pdf = pdf, cdf = cdf, valueSupport = "continuous",
                               variateForm = "univariate",
                               description = paste(fit$dist, "Proportional Hazards Model with log-likelihood",
                                                   fit$loglik[2]),
                               .suppressChecks = TRUE, suppressMoments = TRUE,
                               decorators = c(distr6::CoreStatistics, distr6::ExoticStatistics))
    })
  } else if(type == "aft"){
    distr = lapply(lp, function(x){
      haz = function(x1) exp(-x) * basedist$hazard(x1/exp(x))
      cdf = function(x1) 1 - (basedist$survival(x1/exp(x)))
      pdf = function(x1) haz(x1) * (1 - cdf(x1))
      distr6::Distribution$new(name = paste(fit$dist, "Accelerated Failure Time Model"),
                               short_name = paste0(fit$dist,"AFT"),
                               type = distr6::PosReals$new(), support = distr6::PosReals$new(),
                               pdf = pdf, cdf = cdf, valueSupport = "continuous",
                               variateForm = "univariate",
                               description = paste(fit$dist, "Accelerated Failure Time Model with log-likelihood",
                                                   fit$loglik[2]),
                               .suppressChecks = TRUE, suppressMoments = TRUE,
                               decorators = c(distr6::CoreStatistics, distr6::ExoticStatistics))
    })
  } else if(type == "po"){
    distr = lapply(lp, function(x){
      haz = function(x1) basedist$hazard(x1) * (1 - ( basedist$survival(x1) / ( ((exp(x)-1)^-1) + basedist$survival(x1))))
      cdf = function(x1) 1 - (basedist$survival(x1) * (exp(-x) + (1-exp(-x))*basedist$survival(x1))^-1)
      pdf = function(x1) haz(x1) * (1 - cdf(x1))
      distr6::Distribution$new(name = paste(fit$dist, "Proportional Odds Model"),
                               short_name = paste0(fit$dist,"PO"),
                               type = distr6::PosReals$new(), support = distr6::PosReals$new(),
                               pdf = pdf, cdf = cdf, valueSupport = "continuous",
                               variateForm = "univariate",
                               description = paste(fit$dist, "Proportional Odds Model with log-likelihood",
                                                   fit$loglik[2]),
                               .suppressChecks = TRUE, suppressMoments = TRUE,
                               decorators = c(distr6::CoreStatistics, distr6::ExoticStatistics))
    })
  }

  # crank defined as exponential of linear predictor
  crank = list(crank = as.numeric(exp(lp)))
  lp = list(lp = as.numeric(lp))
  distr = list(distr = distr)

  ret = list()
  if(predict_type %in% c("crank","risk"))
    ret = c(ret, crank = crank)
  else if(predict_type %in% c("lp","link","linear"))
    ret = c(ret, lp = lp)
  else if(predict_type == "distr")
    ret = c(ret, distr = distr)
  else
    ret = c(ret, crank = crank, lp = lp, distr = distr)

  return(ret)
}
