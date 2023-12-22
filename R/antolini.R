cindex = function(pred, meth = c("A", "H"), tiex = 0.5) {
  times = pred$truth[, 1]
  outcome = pred$truth[, 2]

  if (meth == "A") {
    scores = diag(distr6:::C_Vec_WeightedDiscreteCdf(times, sort(unique(times)),
      cdf = t(1 - pred$data$distr), FALSE, FALSE
    ))
  } else {
    scores = p$data$crank
  }

  pairs = data.frame(
    ti = rep(times, length(times)),
    di = rep(outcome, length(times)),
    si = rep(scores, length(times)),
    tj = rep(times, each = length(times)),
    sj = rep(scores, each = length(times))
  )

  comparable = function(t_i, t_j, d_i, c) d_i & t_i < t_j & t_i < c

  comp = comparable(pairs$ti, pairs$tj, pairs$di, Inf)
  if (meth == "A") {
    conc = pairs[comp, "si"] < pairs[comp, "sj"]
  } else {
    conc = pairs[comp, "si"] > pairs[comp, "sj"]
  }
  conc = sum(conc) + sum((pairs[comp, "si"] == pairs[comp, "sj"])) * tiex

  conc / sum(comp)
}

library(mlr3)
library(mlr3proba)
set.seed(42)
t = tsk("rats")
s = partition(t)
p = lrn("surv.coxph")$train(t, s$train)$predict(t, s$test)
p$score(msr("surv.rcll"))
cindex(p, "A", 0.5)


cindex(p, "H", 0.8) - p$score(msr("surv.cindex", tiex = 0.8)) < 1e-6
microbenchmark::microbenchmark(cindex(p, "H", 0.5), p$score())
