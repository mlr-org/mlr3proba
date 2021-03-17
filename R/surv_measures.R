surv_logloss = function(truth, distribution, eps = 1e-15, IPCW = TRUE, train = NULL, ...) {

  # calculate pdf at true death time and set any '0' predictions to a small non-zero value
  pred = as.numeric(distribution$pdf(data = matrix(truth[, 1], nrow = 1)))

  if (IPCW) {
    pred = as.numeric(pred)[truth[, 2] == 1]

    if (is.null(train)) {
      cens = survival::survfit(Surv(truth[, "time"], 1 - truth[, "status"]) ~ 1)
    } else {
      cens = survival::survfit(Surv(train[, "time"], 1 - train[, "status"]) ~ 1)
    }

    truth = truth[truth[, 2] == 1, 1]

    surv = matrix(rep(cens$surv, length(truth)), ncol = length(cens$time), nrow = length(truth),
                    byrow = TRUE)
    distr = .surv_return(times = cens$time, surv = surv)$distr
    pred = as.numeric(distr$survival(data = matrix(truth, nrow = 1)))
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
