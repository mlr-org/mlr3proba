
schmid = function(truth, distribution, times) {
  weighted_survival_score("schmid", truth, distribution, times)
}

graf = function(truth, distribution, times) {
  weighted_survival_score("graf", truth, distribution, times)
}

intslogloss = function(truth, distribution, times, eps = 1e-15){
  weighted_survival_score("intslogloss", truth, distribution, times, eps = eps)
}

weighted_survival_score = function(loss, truth, distribution, times, ...) {
  assert_surv(truth)
  assertDistribution(distribution)

  if(is.null(times)) {
    unique_times = unique(sort(truth[, "time"]))
  } else {
    unique_times = c_get_unique_times(truth[, "time"], times)
  }

  if (loss == "graf") {
    score = c_score_graf(truth[,"time"], unique_times, as.matrix(distribution$cdf(unique_times)))
  } else if {
      score = c_score_schmid(truth[,"time"], unique_times, as.matrix(distribution$cdf(unique_times)))
  } else {
    score = c_score_intslogloss(as.matrix(truth), unique_times,
                              as.matrix(distribution$cdf(unique_times)), ...)
  }

  cens = survival::survfit(Surv(truth[,"time"], 1 - truth[,"status"]) ~ 1)
  score = c_weight_survival_score(score, truth, unique_times, matrix(c(cens$time, cens$surv), ncol = 2))
  colnames(score) = unique_times

  return(score)
}

integrated_score = function(score, integrated, method) {
  if(ncol(score) == 1) {
    integrated = FALSE
  }

  if (integrated) {
    if (method == 1) {
      return(mean(as.numeric(score), na.rm = TRUE))
    } else if (method == 2) {
      times = as.numeric(colnames(score))
      lt = ncol(score)
      score = as.numeric(colMeans(score, na.rm = TRUE))
      return((diff(times) %*% (score[1:(lt - 1)] + score[2:lt])) / (2 * (max(times) - min(times))))
    }
  } else {
    return(colMeans(score, na.rm = TRUE))
  }
}

integrated_se = function(score, integrated) {
  if (integrated) {
    return(sqrt(sum(stats::cov(score)) / (nrow(score) * ncol(score)^2)))
  } else {
    return(apply(score, 2, function(x) stats::sd(x) / sqrt(nrow(score))))
  }
}
