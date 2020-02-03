surv_logloss = function(truth, distribution, eps = 1e-15,...) {
  # get indicator for those not censored
  notcensored = truth[,2] == 1
  # get unique death times for those not censored
  lst = as.list(truth[notcensored, 1])
  names(lst) = paste0("x",1:sum(notcensored))

  # calculate pdf at true death time and set any '0' predictions to a small non-zero value
  pred = as.numeric(do.call(distribution[which(notcensored)]$pdf, lst))
  pred[pred == 0] = eps

  # return negative log-likelihood
  -log(pred)
}

surv_mse = function(truth, response){
  assert_surv(truth)

  uncensored = truth[,2] == 1
  mse = (truth[uncensored,1] - response[uncensored])^2

  list(
    mse = mse,
    se = sd(mse)/sqrt(length(response))
  )
}

surv_mae = function(truth, response){
  assert_surv(truth)

  uncensored = truth[,2] == 1
  mae = abs(truth[uncensored,1] - response[uncensored])

  list(
    mae = mae,
    se = sd(mae)/sqrt(length(response))
  )
}

