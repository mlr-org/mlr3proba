set.seed(1L)
task = tsk("rats")$filter(sample(300, 20L))
learner = suppressWarnings(lrn("surv.coxph")$train(task))
pred = learner$predict(task)
pred$data$response = 1:20
pred$predict_types = c(pred$predict_types, "response")

test_that("mlr_measures", {
  skip_if_not_installed("survAUC")

  keys = mlr_measures$keys("^surv")
  # remove alias for brier
  keys = keys[keys != "surv.graf"]

  for (key in keys) {
    if (grepl("TNR|TPR|tpr|tnr", key)) {
      m = msr(key, times = 60L)
    } else {
      if (key %in% c("surv.intlogloss", "surv.schmid", "surv.brier")) {
        m = msr(key, proper = TRUE)
      } else {
        m = msr(key)
      }
    }

    expect_measure(m)

    expect_silent({
      perf = pred$score(m, task = task, train_set = seq(task$nrow), learner = learner)
    })
    expect_number(perf, na.ok = "na_score" %in% m$properties)

    # test measures with squared-errors
    if (key %in% paste0("surv.", c("schmid", "graf", "intlogloss", "logloss", "mae", "mse",
      "rmse", "calib_alpha", "calib_beta"))) {
      m = suppressWarnings(msr(key, se = TRUE))
      perf = pred$score(m, task = task, train_set = seq(task$nrow), learner = learner)
      expect_number(perf, na.ok = TRUE)
    }
  }
})

learner = suppressWarnings(lrn("surv.coxph")$train(task))
prediction = learner$predict(task)

test_that("unintegrated_prob_losses", {
  msr = msr("surv.logloss")
  expect_silent(prediction$score(msr))
})

test_that("integrated losses with use of times", {
  set.seed(1L)
  t = tsk("rats")$filter(sample(300, 50L))
  p = lrn("surv.kaplan")$train(t)$predict(t)
  losses = paste0("surv.", c("graf", "intlogloss", "schmid"))
  for (loss in losses) {
    m = msr(loss, times = 39:80, integrated = FALSE, proper = TRUE)
    expect_error(p$score(m), "scalar numeric")
  }

  # between 64 and 104
  test_unique_times = sort(unique(prediction$truth[,1]))
  expect_true(all(test_unique_times > 63))
  expect_true(all(test_unique_times < 105))

  # no `times` => use test set's unique time points
  expect_silent(prediction$score(lapply(losses, msr, integrated = TRUE, proper = TRUE)))
  # all `times` outside the test set range
  for (loss in losses) {
    expect_warning(prediction$score(msr(loss, integrated = TRUE, proper = TRUE, times = 34:38)), "requested times")
  }
  # some `times` outside the test set range
  for (loss in losses) {
    expect_warning(prediction$score(msr(loss, integrated = TRUE, proper = TRUE, times = 100:110)), "requested times")
  }
  # one time point, inside the range, no warnings
  expect_silent(prediction$score(lapply(losses, msr, integrated = FALSE, proper = TRUE, times = 80)))
})

test_that("dcalib works", {
  expect_equal(
    pchisq(prediction$score(msr("surv.dcalib", B = 14)), df = 13, lower.tail = FALSE),
    suppressWarnings(prediction$score(msr("surv.dcalib", B = 14, chisq = TRUE)))
  )
})

test_that("calib_beta works", {
  m = msr("surv.calib_beta") # ratio
  expect_equal(m$range, c(-Inf, Inf))
  expect_false(m$minimize)
  expect_false(m$param_set$values$se)
  expect_equal(m$param_set$values$method, "ratio")
  expect_numeric(pred$score(m))

  m2 = msr("surv.calib_beta", method = "diff") # diff
  expect_equal(m2$range, c(0, Inf))
  expect_true(m2$minimize)
  expect_false(m2$param_set$values$se)
  expect_equal(m2$param_set$values$method, "diff")
  expect_numeric(pred$score(m2))
})

test_that("calib_alpha works", {
  m = msr("surv.calib_alpha") # ratio
  expect_equal(m$range, c(-Inf, Inf))
  expect_false(m$minimize)
  expect_false(m$param_set$values$se)
  expect_false(is.finite(m$param_set$values$truncate))
  expect_equal(m$param_set$values$method, "ratio")
  expect_numeric(pred$score(m))

  m2 = msr("surv.calib_alpha", method = "diff") # diff
  expect_equal(m2$range, c(0, Inf))
  expect_true(m2$minimize)
  expect_false(m2$param_set$values$se)
  expect_false(is.finite(m2$param_set$values$truncate))
  expect_equal(m2$param_set$values$method, "diff")
  expect_numeric(pred$score(m2))

  m3 = msr("surv.calib_alpha", method = "diff", truncate = -1)
  expect_equal(m3$param_set$values$truncate, -1)
  expect_equal(unname(pred$score(m3)), -1)
})

test_that("graf training data for weights", {
  m = msr("surv.graf", proper = TRUE)
  t = tsk("rats")
  l = lrn("surv.kaplan")
  s1 = l$train(t, 1:50)$predict(t, 51:100)$score(m)
  s2 = l$train(t, 1:50)$predict(t, 51:100)$score(m, task = t, train_set = 1:50)
  expect_false(identical(s1, s2))
})

test_that("graf proper option", {
  set.seed(1L)
  m1 = msr("surv.graf", proper = TRUE, method = 1)
  m2 = suppressWarnings(msr("surv.graf", proper = FALSE, method = 1))
  l = lrn("surv.kaplan")
  p = l$train(tsk("rats"), row_ids = sample(300, 50))$
    predict(tsk("rats"), row_ids = sample(300, 50))
  s1 = p$score(m1)
  s2 = p$score(m2)
  expect_gt(s2, s1)
})

test_that("graf with 1 time point", {
  data = data.frame(time = c(1,1), status = c(1,0), f1 = c(5,3))
  task = as_task_surv(x = data, event = "status", time = "time")
  res = suppressWarnings(lrn("surv.coxph")$train(task)$predict(task))
  expect_number(res$score(msr("surv.graf", times = 1)))
})

test_that("graf: t_max, p_max, times", {
  set.seed(1L)
  t = tsk("rats")$filter(sample(1:300, 50))
  p = lrn("surv.kaplan")$train(t)$predict(t)

  expect_error(p$score(msr("surv.graf", integrated = FALSE, times = 1:2)))
  expect_error(p$score(msr("surv.graf", integrated = FALSE)))
  expect_error(p$score(msr("surv.graf", times = 1:2, t_max = 3)))

  times = sort(unique(p$truth[,1])) # test time points
  t_max = 100
  times_flt = times[times <= t_max] # keep only times until the `t_max`
  m0 = p$score(msr("surv.graf")) # uses all test time points
  m1 = p$score(msr("surv.graf", times = times_flt)) # uses times_flt
  m2 = p$score(msr("surv.graf", t_max = t_max)) # 100
  m3 = p$score(msr("surv.graf", t_max = max(times))) # 104
  m4 = p$score(msr("surv.graf", t_max = max(times) + 1)) # 105

  # different time points considered
  expect_true(m0 != m1)
  # same time points are used, but `t_max` also removes observations
  expect_true(m1 != m2)
  # different `t_max` => different time points used
  expect_true(m2 != m3)
  # different `t_max` but after the max evaluation time point, so result stays the same
  expect_equal(m3, m4)

  s = t$kaplan() # KM
  t_max = s$time[which(1 - s$n.risk / s$n > 0.3)[1]] # t_max for up to 30% cens

  # graf score: t_max and p_max are the same
  m1 = p$score(msr("surv.graf", t_max = t_max))
  m2 = p$score(msr("surv.graf", p_max = 0.3))
  m3 = p$score(msr("surv.graf", p_max = 0.5))
  expect_equal(m1, m2)
  expect_true(m1 != m3)

  # times is not necessarily decomposable, due to the `method` that performs integration
  p_cox = suppressWarnings(lrn("surv.coxph")$train(t)$predict(t))
  s1 = p_cox$score(msr("surv.graf", times = 68))
  s2 = p_cox$score(msr("surv.graf", times = 92))
  s3 = p_cox$score(msr("surv.graf", times = 102))
  mean_score = (s1 + s2 + s3) / 3
  # simple mean
  s_all1 = p_cox$score(msr("surv.graf", method = 1, times = c(68, 92, 102)))
  # mean weighted by the difference between time-points
  s_all2 = p_cox$score(msr("surv.graf", method = 2, times = c(68, 92, 102)))
  expect_equal(s_all1, mean_score)
  expect_true(s_all2 != mean_score)
})

test_that("cindex: t_max, p_max", {
  set.seed(1L)
  t = tsk("rats")$filter(sample(1:300, 50))
  s = t$kaplan() # KM
  t_max = s$time[which(1 - s$n.risk / s$n > 0.3)[1]] # t_max for up to 30% cens

  # t_max and p_max are the same
  p_cox = suppressWarnings(lrn("surv.coxph")$train(t)$predict(t))
  c1 = p_cox$score(msr("surv.cindex", t_max = t_max))
  c2 = p_cox$score(msr("surv.cindex", p_max = 0.3))
  c3 = p_cox$score(msr("surv.cindex", p_max = 0.5))
  expect_equal(c1, c2)
  expect_true(c1 != c3)
})

test_that("ERV works as expected", {
  set.seed(1L)
  t = tsk("rats")
  part = partition(t, 0.8)
  l = lrn("surv.kaplan")
  p = l$train(t, part$train)$predict(t, part$test)
  m = msr("surv.graf", ERV = TRUE)
  # KM is the baseline score, so ERV score = 0
  expect_equal(as.numeric(p$score(m, task = t, train_set = part$train)), 0)

  l = lrn("surv.coxph")
  p = l$train(t, part$train)$predict(t, part$test)
  m = msr("surv.graf", ERV = TRUE)
  # Cox should do a little better than the KM baseline (ERV score > 0)
  expect_gt(as.numeric(p$score(m, task = t, train_set = part$train)), 0)

  # some checks
  set.seed(1L)
  t = tsk("rats")$filter(sample(1:300, 50))
  l = lrn("surv.kaplan")
  p = l$train(t)$predict(t)
  m = msr("surv.graf", ERV = TRUE, se = TRUE)
  expect_error(p$score(m), "'task'")
  expect_error(p$score(m, task = t, train_set = t$row_ids), "`se`")
})

test_that("ERV=TRUE changes some measure fields", {
  m = msr("surv.rcll")
  m_erv = msr("surv.rcll", ERV = TRUE)
  expect_false(m$param_set$values$ERV)
  expect_true(m$minimize)
  expect_equal(m$range, c(0, Inf))
  expect_true(m_erv$param_set$values$ERV)
  expect_false(m_erv$minimize)
  expect_equal(m_erv$range, c(-Inf, 1))

  m = msr("surv.graf")
  m_erv = msr("surv.graf", ERV = TRUE)
  expect_false(m$param_set$values$ERV)
  expect_true(m$minimize)
  expect_equal(m$range, c(0, Inf))
  expect_true(m_erv$param_set$values$ERV)
  expect_false(m_erv$minimize)
  expect_equal(m_erv$range, c(-Inf, 1))

  m = msr("surv.intlogloss")
  m_erv = msr("surv.intlogloss", ERV = TRUE)
  expect_false(m$param_set$values$ERV)
  expect_true(m$minimize)
  expect_equal(m$range, c(0, Inf))
  expect_true(m_erv$param_set$values$ERV)
  expect_false(m_erv$minimize)
  expect_equal(m_erv$range, c(-Inf, 1))

  m = msr("surv.logloss")
  m_erv = msr("surv.logloss", ERV = TRUE)
  expect_false(m$param_set$values$ERV)
  expect_true(m$minimize)
  expect_equal(m$range, c(0, Inf))
  expect_true(m_erv$param_set$values$ERV)
  expect_false(m_erv$minimize)
  expect_equal(m_erv$range, c(-Inf, 1))

  m = msr("surv.schmid")
  m_erv = msr("surv.schmid", ERV = TRUE)
  expect_false(m$param_set$values$ERV)
  expect_true(m$minimize)
  expect_equal(m$range, c(0, Inf))
  expect_true(m_erv$param_set$values$ERV)
  expect_false(m_erv$minimize)
  expect_equal(m_erv$range, c(-Inf, 1))
})

test_that("rcll works", {
  set.seed(1L)
  t = tsk("rats")$filter(sample(1:300, 50))
  l = lrn("surv.kaplan")
  p = l$train(t)$predict(t)
  p2 = p$clone()
  p2$data$distr = p2$distr # hack: test score via distribution
  m = msr("surv.rcll")
  expect_true(m$minimize)
  expect_equal(m$range, c(0, Inf))
  KMscore = p$score(m)
  expect_numeric(KMscore)
  KMscore2 = p2$score(m)
  expect_equal(KMscore, KMscore2)

  status = t$truth()[, 2L]
  row_ids = t$row_ids
  cens_ids = row_ids[status == 0]
  event_ids = row_ids[status == 1]

  # only censored rats in test set
  p = l$predict(t, row_ids = cens_ids)
  score = p$score(m)
  expect_numeric(score)
  p2 = p$clone() # test score via distribution
  p2$data$distr = p2$distr
  score2 = p2$score(m)
  expect_equal(score, score2)

  # 1 censored test rat
  p = p$filter(row_ids = cens_ids[1L])
  score = p$score(m)
  expect_numeric(score)
  p2 = p$clone() # test score via distribution
  p2$data$distr = p2$distr
  score2 = p2$score(m)
  expect_equal(score, score2)

  # only dead rats in test set
  p = l$predict(t, row_ids = event_ids)
  score = p$score(m)
  expect_numeric(score)
  p2 = p$clone() # test score via distribution
  p2$data$distr = p2$distr # Matdist(1xY)
  score2 = p2$score(m)
  expect_equal(score, score2)

  # 1 dead rat
  p = p$filter(row_ids = event_ids[1L])
  score = p$score(m)
  expect_numeric(score)
  p2 = p$clone() # test score via distribution
  p2$data$distr = p2$distr[1L] # WeightDisc
  score2 = p2$score(m)
  expect_equal(score, score2)

  # Cox is better than baseline (Kaplan-Meier)
  l2 = lrn("surv.coxph")
  p2 = suppressWarnings(l2$train(t)$predict(t))
  expect_lt(p2$score(m), KMscore)

  # Another edge case: some dead rats and 1 only censored
  p3 = p2$filter(row_ids = c(event_ids, cens_ids[1L]))
  score = p3$score(m)
  expect_numeric(score)
  p3$data$distr = p3$distr
  score2 = p3$score(m)
  expect_equal(score, score2)
})

test_that("dcal works", {
  set.seed(1L)
  t = tsk("rats")$filter(sample(1:300, 50))
  l = lrn("surv.coxph")
  p = suppressWarnings(l$train(t)$predict(t))
  p2 = p$clone()
  p2$data$distr = p2$distr # hack: test score via distribution
  m = msr("surv.dcalib", truncate = 20)
  expect_true(m$minimize)
  expect_equal(m$range, c(0, Inf))
  expect_equal(m$param_set$values$B, 10)
  expect_false(m$param_set$values$chisq)
  expect_equal(m$param_set$values$truncate, 20)
  KMscore = p$score(m)
  expect_numeric(KMscore)
  KMscore2 = p2$score(m)
  expect_equal(KMscore, KMscore2)

  status = t$truth()[, 2L]
  row_ids = t$row_ids
  cens_ids = row_ids[status == 0]
  event_ids = row_ids[status == 1]

  # only censored rats in test set
  p = l$predict(t, row_ids = cens_ids)
  score = p$score(m)
  expect_numeric(score)
  p2 = p$clone() # test score via distribution
  p2$data$distr = p2$distr
  score2 = p2$score(m)
  expect_equal(score, score2)

  # 1 censored test rat
  p = p$filter(row_ids = cens_ids[1L])
  score = p$score(m)
  expect_numeric(score)
  p2 = p$clone() # test score via distribution
  p2$data$distr = p2$distr
  score2 = p2$score(m)
  expect_equal(score, score2)

  # only dead rats in test set
  p = l$predict(t, row_ids = event_ids)
  score = p$score(m)
  expect_numeric(score)
  p2 = p$clone() # test score via distribution
  p2$data$distr = p2$distr # Matdist(1xY)
  score2 = p2$score(m)
  expect_equal(score, score2)

  # 1 dead rat
  p = p$filter(row_ids = event_ids[1L])
  score = p$score(m)
  expect_numeric(score)
  p2 = p$clone() # test score via distribution
  p2$data$distr = p2$distr[1L] # WeightDisc
  score2 = p2$score(m)
  expect_equal(score, score2)

  # Another edge case: some dead rats and 1 only censored
  p = l$predict(t, row_ids = c(event_ids, cens_ids[1L]))
  score = p$score(m)
  expect_numeric(score)
  p$data$distr = p$distr
  score2 = p$score(m)
  expect_equal(score, score2)
  expect_gt(score, 10)

  score3 = p$score(msr("surv.dcalib", truncate = 10))
  expect_equal(unname(score3), 10)
  score4 = p$score(msr("surv.dcalib", truncate = 5))
  expect_equal(unname(score4), 5)
  score5 = p$score(msr("surv.dcalib", truncate = Inf, B = 20)) # B affects truncate
  expect_gt(score5, score)
})

test_that("logloss works", {
  set.seed(1L)
  t = tsk("rats")$filter(sample(1:300, 50))
  l = lrn("surv.kaplan")
  p = l$train(t)$predict(t)
  p2 = p$clone()
  p2$data$distr = p2$distr # hack: test score via distribution
  m = msr("surv.logloss") # IPCW = TRUE (RNLL)
  m2 = msr("surv.logloss", IPCW = FALSE) # NLL
  expect_true(m$minimize)
  expect_equal(m$range, c(0, Inf))
  KMscore = p$score(m)
  expect_numeric(KMscore)
  KMscore2 = p2$score(m)
  expect_equal(KMscore, KMscore2)

  status = t$truth()[, 2L]
  row_ids = t$row_ids
  cens_ids = row_ids[status == 0]
  event_ids = row_ids[status == 1]

  # only censored rats in test set
  p = l$predict(t, row_ids = cens_ids)
  expect_true(is.nan(p$score(m))) # NaN
  expect_false(is.nan(p$score(m2)))
  p2 = p$clone() # test score via distribution
  p2$data$distr = p2$distr
  expect_true(is.nan(p2$score(m))) # NaN
  expect_false(is.nan(p2$score(m2)))

  # 1 censored test rat
  p = p$filter(row_ids = cens_ids[1L])
  expect_true(is.nan(p$score(m)))
  expect_false(is.nan(p$score(m2)))
  p2 = p$clone() # test score via distribution
  p2$data$distr = p2$distr
  expect_true(is.nan(p2$score(m)))
  expect_false(is.nan(p2$score(m2)))

  # only dead rats in test set
  p = l$predict(t, row_ids = event_ids)
  score = p$score(m)
  expect_numeric(score)
  expect_equal(score, p$score(m2)) # as G_km(t) is 1
  p2 = p$clone() # test score via distribution
  p2$data$distr = p2$distr # Matdist(1xY)
  score2 = p2$score(m)
  expect_equal(score, score2)

  # 1 dead rat
  p = p$filter(row_ids = event_ids[1L])
  score = p$score(m)
  expect_numeric(score)
  expect_equal(score, p$score(m2))
  p2 = p$clone() # test score via distribution
  p2$data$distr = p2$distr[1L] # WeightDisc
  score2 = p2$score(m)
  expect_equal(score, score2)

  # Cox is better than baseline (Kaplan-Meier)
  l2 = lrn("surv.coxph")
  p2 = suppressWarnings(l2$train(t)$predict(t))
  expect_lt(p2$score(m), KMscore)

  # Another edge case: some dead rats and 1 only censored
  p3 = p2$clone()$filter(row_ids = c(event_ids, cens_ids[1L]))
  score = p3$score(m)
  expect_numeric(score)
  expect_true(score != p3$score(m2)) # since dead rats are removed
  p3$data$distr = p3$distr
  score2 = p3$score(m)
  expect_equal(score, score2)

  # Another edge case: some censored rats and 1 only dead
  p4 = p2$clone()$filter(row_ids = c(cens_ids, event_ids[1L]))
  score = p4$score(m)
  expect_numeric(score)
  expect_true(score != p4$score(m2)) # since the dead rat is removed
  p4$data$distr = p4$distr
  score2 = p4$score(m)
  expect_equal(score, score2)
})

test_that("distr measures work with 3d survival array", {
  learner = lrn("surv.kaplan")$train(task)
  p = learner$predict(task)
  expect_matrix(p$data$distr)
  expect_r6(p$distr, "Matdist")

  # hack: substitute with 3d survival array
  p = reshape_distr_to_3d(p)
  expect_array(p$data$distr)
  expect_r6(p$distr, "Arrdist") # `distr6` interface class changed

  distr_msrs = msrs(as.data.table(mlr_measures)[
    predict_type == "distr" & startsWith(key, "surv")
  ]$key)

  for (m in distr_msrs) {
    expect_numeric({
      p$score(m, task = task, train_set = seq(task$nrow), learner = learner)
    })
  }
})
