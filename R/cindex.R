cindex = function(truth, crank, cutoff = NULL,
                  weight_meth = c("I", "G", "G2", "SG", "S"),
                  tiex = 0.5, train = NULL) {

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
    cens = matrix()
  } else {
    cens = survival::survfit(Surv(train[,"time"], 1 - train[,"status"]) ~ 1)
    cens = matrix(c(cens$time, cens$surv), ncol = 2)
  }

  if (weight_meth == "SG" | weight_meth == "S") {
    surv = survival::survfit(train ~ 1)
    surv = matrix(c(surv$time, surv$surv), ncol = 2)
  } else {
    surv = matrix()
  }

  if (is.null(cutoff)) {
    cutoff = max(time) + 1
  }

  c_concordance(time, status, crank[ord], cutoff, weight_meth, cens, surv, tiex)
}
