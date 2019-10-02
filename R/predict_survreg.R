predict_survreg = function(object, task, type = "aft", predict_type = "all"){

  basedist = object$basedist
  fit = object$fit

  distr6::testDistribution(basedist)
  assertClass(fit, "survreg")

  newdata = task$data(cols = task$feature_names)

  x = stats::model.matrix(formulate(rhs = task$feature_names), data = newdata, xlev = task$levels())[,-1]

  lp = matrix(fit$coefficients[-1], nrow = 1) %*% t(x)

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
    risk = exp(lp)
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
    risk = exp(-lp)
  } else if(type == "odds"){
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
    risk = exp(lp)
  }

  if(predict_type == "risk")
    return(as.numeric(risk))
  else if(predict_type %in% c("lp","link","linear"))
    return(as.numeric(lp))
  else if(predict_type == "distr")
    return(distr)
  else
    return(list(risk = as.numeric(risk), distr = distr, lp = as.numeric(lp)))
}
