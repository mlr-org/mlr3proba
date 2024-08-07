cindex = function(truth, crank, t_max = NULL,
  weight_meth = c("I", "G", "G2", "SG", "S"),
  tiex = 0.5, train = NULL, eps = 1e-3) {

  if (length(unique(crank)) == 1L) {
    return(0.5)
  }

  assert_surv(truth)
  assert_numeric(crank)
  if (anyMissing(truth)) {
    return(NA_real_)
  }

  ord = order(truth[, "time"])
  time = as.double(truth[, "time"])[ord]
  status = as.integer(truth[, "status"])[ord]

  weight_meth = match.arg(weight_meth)

  if (weight_meth %in% c("I", "S")) {
    cens = matrix(ncol = 2L)
  } else {
    cens = survival::survfit(Surv(train[, "time"], 1 - train[, "status"]) ~ 1)
    cens = matrix(c(cens$time, cens$surv), ncol = 2L)
  }

  if (weight_meth == "SG" || weight_meth == "S") {
    surv = survival::survfit(train ~ 1)
    surv = matrix(c(surv$time, surv$surv), ncol = 2L)
  } else {
    surv = matrix(ncol = 2L)
  }

  if (is.null(t_max)) {
    t_max = max(time) + 1
  }

  cens[cens[, 2L] == 0, 2L] = eps
  surv[surv[, 2L] == 0, 2L] = eps

  c_concordance(time, status, crank[ord], t_max, weight_meth, cens, surv, tiex)
}
