cindex = function(pred, meth = c("A", "H"), tiex = 0.5) {
  all_times = pred$truth[, 1] # to differentiate with `times` below
  status = pred$truth[, 2]

  # we write some code for Arrdist or VectorDistribution as in other measures
  # in the end we just need the survival matrix
  surv = pred$data$distr
  times = as.numeric(colnames(surv))

  if (meth == "A") {
    extend_times = getFromNamespace("C_Vec_WeightedDiscreteCdf", ns = "distr6")
    scores = diag(
      extend_times(x = all_times, data = times, cdf = t(1 - surv), FALSE, FALSE)
      # extend_times should return square matrix always for what we aim to do
    )
    # edge case to fix: `t(1 - surv)` does not return matrix if surv is 1x() or ()x1
  } else {
    scores = unname(pred$data$crank)
  }

  pairs = data.frame(
    ti = rep(all_times, length(all_times)),
    di = rep(status, length(all_times)),
    si = rep(scores, length(all_times)),
    tj = rep(all_times, each = length(all_times)),
    sj = rep(scores, each = length(all_times))
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
p$score()
cindex(p, "H")
cindex(p, "A", 0.5)

cindex(p, "H", 0.8) - p$score(msr("surv.cindex", tiex = 0.8))
cindex(p, "H", 0.8) - p$score(msr("surv.cindex", tiex = 0.8)) < 1e-6
microbenchmark::microbenchmark(cindex(p, "H", 0.5), p$score())
