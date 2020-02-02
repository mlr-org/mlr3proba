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
  uncensored = truth[,2] == 1

  list(
    mse = (prediction$truth[uncensored,1] - prediction$response[uncensored])^2,
    se = sd(mse)/sqrt(length(response))
  )
}

