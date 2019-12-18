weighted_logloss = function(truth, distribution, times, eps = 1e-15,...){
  # unweighted logloss score at time t* as L(t*) = -I(t > t*)log(S(t*)) - I(t <= t*)log(F(t*))
  logloss = function(alive, distribution, unique_times, eps){
    # if a patient is alive at t then find the survival, otherwise find cdf
    surv = transpose(1 - distribution$cdf(unique_times))
    ll = (surv * alive) + ((1 - surv) * (1 - alive))
    # set prediction to be very small but non-zero then find negative log
    ll[ll == 0] = eps

    -log(ll)
  }

  weighted_survival_score(truth, distribution, times, logloss, eps = eps)
}

weighted_graf = function(truth, distribution, times, ...) {
  # unweighted graf score at time t* as G(t*) = (I(t > t*) - S(t*))^2
  graf = function(alive, distribution, unique_times) (alive - transpose(1 - distribution$cdf(unique_times)))^2

  weighted_survival_score(truth, distribution, times, graf)
}

integrated_score = function(score, integrated) {
  if (integrated) {
    return(mean(as.numeric(score), na.rm = TRUE))
  } else {
    return(colMeans(score, na.rm = TRUE))
  }
}

integrated_se = function(score, integrated) {
  if (integrated) {
    return(sqrt(sum(stats::cov(score))/(nrow(score) * ncol(score)^2)))
  } else {
    return(apply(score, 2, function(x) stats::sd(x)/sqrt(nrow(score))))
  }
}
