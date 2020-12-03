surv_logloss = function(truth, distribution, rm.cens, eps, weight_meth, task, train_set, ...) {

  # calculate pdf at true death time and set any '0' predictions to a small non-zero value
  pred = distribution$pdf(data = matrix(truth[, 1], nrow = 1))

  if (rm.cens) {
    times = truth[truth[, 2] == 1, 1]
    pred = as.numeric(pred)[truth[, 2] == 1]
  } else {
    times = truth[, 1]
  }

  pred[pred == 0] = eps

  if (weight_meth == "G") {
    cens = survival::survfit(Surv(task$truth(train_set)[, 1L], 1 - task$truth(train_set)[, 2L]) ~ 1)
    # return weighted negative log-likelihood
    return(-log(pred)/cens$surv[findInterval(times, cens$time)])
  } else {
    # return negative log-likelihood
    return(-log(pred))
  }
}

surv_mse = function(truth, response) {
  assert_surv(truth)

  uncensored = truth[, 2] == 1
  mse = (truth[uncensored, 1] - response[uncensored])^2

  list(
    mse = mse,
    se = sd(mse) / sqrt(length(response))
  )
}

surv_mae = function(truth, response) {
  assert_surv(truth)

  uncensored = truth[, 2] == 1
  mae = abs(truth[uncensored, 1] - response[uncensored])

  list(
    mae = mae,
    se = sd(mae) / sqrt(length(response))
  )
}
