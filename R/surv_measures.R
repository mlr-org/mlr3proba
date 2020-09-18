surv_logloss = function(truth, distribution, rm.cens = TRUE, eps = 1e-15, ...) {

  # calculate pdf at true death time and set any '0' predictions to a small non-zero value
  pred = distribution$pdf(data = matrix(truth[, 1], nrow = 1))

  if (rm.cens) {
    pred = as.numeric(pred)[truth[, 2] == 1]
  }

  pred[pred == 0] = eps

  # return negative log-likelihood
  -log(pred)
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
