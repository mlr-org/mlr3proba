cindex_chatGTP = function(pred, meth = c("A", "H"), tiex = 0.5, cutoff = Inf) {
  #browser()
  n_obs = length(pred$truth)
  pred_times = pred$truth[, 1]
  status = pred$truth[, 2]
  surv = pred$data$distr
  times = as.numeric(colnames(surv))
  risk = unname(pred$data$crank)

  # Assuming meth "A" optimization
  if (meth == "A") {
    extend_times = getFromNamespace("C_Vec_WeightedDiscreteCdf", ns = "distr6")
    surv_mat = extend_times(x = pred_times, data = times, cdf = t(1 - surv), FALSE, FALSE)
    rownames(surv_mat) = pred_times
  }

  n_seq = seq_len(n_obs)
  pairs_i = rep(n_seq, each = n_obs)
  pairs_j = rep(n_seq, n_obs)

  comparable = function(ti, tj, di, cutoff) di & ti < tj & ti < cutoff
  comp = comparable(pred_times[pairs_i], pred_times[pairs_j], status[pairs_i], cutoff = cutoff)

  if (meth == "A") {
    surv_ii2 = rep(diag(surv_mat), times = n_obs)
    surv_ij = surv_mat[cbind(as.character(pred_times[pairs_i]), pairs_j)]

    conc = sum(surv_ii2[comp] < surv_ij[comp]) +
           sum(surv_ii2[comp] == surv_ij[comp]) * tiex
  } else {
    ri = risk[pairs_i]
    rj = risk[pairs_j]
    conc = sum(ri[comp] > rj[comp]) +
           sum(ri[comp] == rj[comp]) * tiex
  }

  sprintf("Numerator: %f, Denominator: %f", conc, sum(comp))
  conc / sum(comp)
}

cindex_fast = function(pred, meth = c("A", "H"), tiex = 0.5, cutoff = Inf) {
  #browser()
  n_obs = length(pred$truth)
  test_times  = pred$truth[, 1] # to differentiate with `times` below
  test_status = pred$truth[, 2]

  surv = pred$data$distr # survival matrix
  pred_times = as.numeric(colnames(surv))
  risk = unname(pred$data$crank)

  if (meth == "A") {
    extend_times = getFromNamespace("C_Vec_WeightedDiscreteCdf", ns = "distr6")
    surv_mat = extend_times(x = test_times, data = pred_times, cdf = t(1 - surv), FALSE, FALSE)
    # add time points (important for indexing)
    rownames(surv_mat) = test_times

    # extend_times should return square matrix always
    # edge case to fix: `t(1 - surv)` does not return matrix if surv is 1x() or ()x1
  }

  #browser()
  pairs = data.frame(
    # i obs
    i  = seq_len(n_obs),
    ti = rep(test_times, n_obs),
    di = rep(test_status, n_obs),
    ri = rep(risk, n_obs),
    # j obs
    j  = rep(seq_len(n_obs), each = n_obs),
    tj = rep(test_times, each = n_obs),
    rj = rep(risk, each = n_obs)
  )

  comparable = function(ti, tj, di, cutoff) di & ti < tj & ti < cutoff

  comp = comparable(pairs$ti, pairs$tj, pairs$di, cutoff = cutoff)
  sprintf("Comparable: %d", comp)
  if (meth == "A") {
    # add the S(i)(Ti) and S(j)(Ti) for Antolini's C-index
    # S is survival matrix with: rows => times, cols => obs
    surv_ii = sapply(1:nrow(pairs), function(row_index) {
      row = pairs[row_index, ]
      unname(surv_mat[as.character(row[, "ti"]), row[, "i"]])
    }) # survival of i-th obs at T(i) event time point
    # same as:
    surv_ii2 = rep(diag(surv_mat), times = n_obs) # much faster, keep that!
    testthat::expect_equal(surv_ii, surv_ii2)

    surv_ij = sapply(1:nrow(pairs), function(row_index) {
      row = pairs[row_index, ]
      unname(surv_mat[as.character(row[, "ti"]), row[, "j"]])
    }) # survival of j-th obs at T(i) event time point

    # fill in the survival probability columns
    pairs = cbind(pairs, sii = surv_ii, sij = surv_ij)

    conc = pairs[comp, "sii"] < pairs[comp, "sij"]
    conc = sum(conc) + sum((pairs[comp, "sii"] == pairs[comp, "sij"])) * tiex
  } else {
    conc = pairs[comp, "ri"] > pairs[comp, "rj"]
    conc = sum(conc) + sum((pairs[comp, "ri"] == pairs[comp, "rj"])) * tiex
  }

  sprintf("Numerator: %f, Denominator: %f", conc, sum(comp))
  conc / sum(comp)
}

library(mlr3)
library(mlr3proba)

set.seed(42)
task = tsk("rats")
part = partition(task)
p = lrn("surv.coxph")$train(task, part$train)$predict(task, part$test)

# smaller example
p2 = p$clone()$filter(row_ids = c(7, 11, 39, 79, 100))
y = p2$truth
x = unname(p2$crank)

p2$score()
cindex(y, x, t_max = NULL, weight_meth = "I", tiex = 0.5, train = NULL, eps = 1e-3)
cindex_fast(p2, "H")
cindex_chatGTP(p2, "A") # subscript out of bounds
survival::concordance(y ~ x, reverse = TRUE)

# larger example
p$score()
y = p$truth
x = unname(p$crank)
cindex_fast(p, "H")
cindex_chatGTP(p, "H")
y = p$truth
x = unname(p$crank)
survival::concordance(y ~ x, reverse = TRUE)

p$score(msr("surv.cindex", tiex = 1))
cindex_fast(p, "H", tiex = 1)

# check Antolini
cindex_fast(p, "A", 0.5)

cindex_fast(p, "H", 0.8) - p$score(msr("surv.cindex", tiex = 0.8)) # < 1e-6
microbenchmark::microbenchmark(cindex_fast(p, "H", 0.5), p$score()) # faster

bench::mark()

# benchmark check
set.seed(42)
bmr = benchmark(benchmark_grid(
  tasks = tsks(c("rats", "gbcs", "grace")),
  learners = lrn("surv.coxph"),
  resamplings = rsmp("cv", folds = 3)
))
bmr$score()$surv.cindex # > 0.7

# slow! but all Antolini's C > 0.7 (correct implementation)
# some are equal to Harrell's C, some are not? (due 1-1 correspondence
# they all should be?)
for (i in 1:3) {
  for (p in bmr$resample_results$resample_result[[i]]$predictions()) {
    print(cindex_fast(pred = p, meth = "A")) # "H" => checking Harrell's C is the same as above (YES)
  }
}
