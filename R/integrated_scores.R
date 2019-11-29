weighted_logloss = function(truth, distribution, eps = 1e-15,...){
  # unweighted logloss score at time t* as L(t*) = -I(t > t*)log(S(t*)) - I(t <= t*)log(F(t*))
  logloss = function(alive, distribution, unique_times, eps){
    # if a patient is alive at t then find the survival, otherwise find cdf
    surv = transpose(1 - distribution$cdf(unique_times))
    ll = (surv * alive) + ((1 - surv) * (1 - alive))
    # set prediction to be very small but non-zero then find negative log
    ll[ll == 0] = eps

    -log(ll)
  }

  weighted_survival_score(truth, distribution, logloss, eps = eps)
}

weighted_graf = function(truth, distribution,...) {
  # unweighted graf score at time t* as G(t*) = (I(t > t*) - S(t*))^2
  graf = function(alive, distribution, unique_times) (alive - transpose(1 - distribution$cdf(unique_times)))^2

  weighted_survival_score(truth, distribution, graf)
}

integrated_score = function(score, integrated, times) {
  if (integrated) {
    return(mean(as.numeric(score), na.rm = TRUE))
  } else {
    if(length(times) == 0) {
      score = score[,findInterval(times, as.numeric(colnames(score)), all.inside = TRUE), drop = FALSE]
    } else {
      return(colMeans(score, na.rm = TRUE))
    }
  }
}

integrated_se = function(score, integrated, times) {
  if (integrated) {
    return(sqrt(sum(stats::cov(score))/(prod(dim(score)))^2))
  } else {
    if(length(times) != 0) {
      score = score[,findInterval(times, as.numeric(colnames(score)), all.inside = TRUE), drop = FALSE]
    } else {
      return(apply(score, 2, function(x) stats::sd(x)/sqrt(length(x))))
    }
  }
}
