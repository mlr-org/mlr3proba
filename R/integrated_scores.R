integrated_logloss = function(truth, distribution, eps = 1e-15,...){
  # unweighted logloss score at time t* as L(t*) = (I(t > t*) - S(t*))^2
  logloss = function(alive, distribution, eps){
    # if a patient is alive at t then find the survival, otherwise find cdf
    ll = (surv * alive) + ((1 - surv) * (1 - alive))
    # set prediction to be very small but non-zero then find negative log
    ll[ll == 0] = eps

    -log(ll)
  }

  weighted_survival_score(truth, distribution, logloss, eps = eps)
}

integrated_graf = function(truth, distribution,...) {
  # unweighted graf score at time t* as G(t*) = (I(t > t*) - S(t*))^2
  graf = function(alive, distribution) (alive - transpose(1 - distribution$cdf(unique_times)))^2

  weighted_survival_score(truth, distribution, graf)
}
