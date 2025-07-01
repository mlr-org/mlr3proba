# construct `pred` object with all possible predict types for testing
set.seed(1L)
task = tsk("rats")$filter(sample(300, 50L))
learner = suppressWarnings(lrn("surv.coxph")$train(task))
pred = learner$predict(task)
pred$data$response = 1:50
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
      m = msr(key)
    }

    expect_measure(m)

    expect_silent({
      perf = pred$score(m, task = task, train_set = task$row_ids, learner = learner)
    })
    expect_number(perf, na.ok = "na_score" %in% m$properties)
  }
})

test_that("integrated losses + times", {
  set.seed(1L)
  t = tsk("rats")$filter(sample(300, 50L))
  p = lrn("surv.kaplan")$train(t)$predict(t)
  losses = paste0("surv.", c("graf", "intlogloss", "schmid"))
  for (loss in losses) {
    m = msr(loss, times = 39:80, integrated = FALSE)
    expect_error(p$score(m), "scalar numeric")
  }

  # between 64 and 104
  test_unique_times = sort(unique(pred$truth[,1]))
  expect_true(all(test_unique_times > 38))
  expect_true(all(test_unique_times < 105))

  # no `times` => use test set's unique time points
  expect_silent(pred$score(lapply(losses, msr, integrated = TRUE)))
  # all `times` outside the test set range
  for (loss in losses) {
    expect_warning(pred$score(msr(loss, integrated = TRUE, times = 34:38)), "requested times")
  }
  # some `times` outside the test set range
  for (loss in losses) {
    expect_warning(pred$score(msr(loss, integrated = TRUE, times = 100:110)), "requested times")
  }
  # one time point, inside the range, no warnings
  expect_silent(pred$score(lapply(losses, msr, integrated = FALSE, times = 80)))
})

test_that("dcalib works", {
  # see if chisq statistic and associated p-value are correctedly calculated
  dc1 = msr("surv.dcalib", B = 14, chisq = FALSE) # default
  dc2 = msr("surv.dcalib", B = 14, chisq = TRUE)
  expect_equal(
    pchisq(pred$score(dc1), df = 13, lower.tail = FALSE),
    suppressWarnings(pred$score(dc2))
  )

  set.seed(1L)
  t = tsk("rats")$filter(sample(1:300, 50))
  l = lrn("surv.coxph")
  p = suppressWarnings(l$train(t)$predict(t))

  m = msr("surv.dcalib", truncate = 20)
  expect_true(m$minimize)
  expect_equal(m$range, c(0, Inf))
  expect_equal(m$param_set$values$B, 10)
  expect_false(m$param_set$values$chisq)
  expect_equal(m$param_set$values$truncate, 20)
  KMscore = p$score(m)
  expect_numeric(KMscore)

  status = t$truth()[, 2L]
  row_ids = t$row_ids
  cens_ids = row_ids[status == 0]
  event_ids = row_ids[status == 1]

  # only censored rats in test set
  p = l$predict(t, row_ids = cens_ids)
  score = p$score(m)
  expect_numeric(score)

  # 1 censored test rat
  p = p$filter(row_ids = cens_ids[1L])
  score = p$score(m)
  expect_numeric(score)

  # only dead rats in test set
  p = l$predict(t, row_ids = event_ids)
  score = p$score(m)
  expect_numeric(score)

  # 1 dead rat
  p = p$filter(row_ids = event_ids[1L])
  score = p$score(m)
  expect_numeric(score)

  # Another edge case: some dead rats and 1 only censored
  p = l$predict(t, row_ids = c(event_ids, cens_ids[1L]))
  score = p$score(m)
  expect_numeric(score)
  expect_gt(score, 10)

  score3 = p$score(msr("surv.dcalib", truncate = 10))
  expect_equal(unname(score3), 10)
  score4 = p$score(msr("surv.dcalib", truncate = 5))
  expect_equal(unname(score4), 5)
  score5 = p$score(msr("surv.dcalib", truncate = Inf, B = 20)) # B affects truncate
  expect_gt(score5, score)
})

test_that("mae/mse/rmse work", {
  is_event = pred$truth[, 2L] == 1
  event_times = pred$truth[is_event, 1L]
  surv_times  = pred$response[is_event]
  errors = event_times - surv_times
  mae = pred$score(msr("surv.mae"))
  mse = pred$score(msr("surv.mse"))
  rmse = pred$score(msr("surv.rmse"))

  expect_equal(unname(mae), mean(abs(errors)))
  expect_equal(unname(mse), mean(errors^2))
  expect_equal(unname(rmse), sqrt(mean(errors^2)))

  # only censored observations => return NA
  cens_ids = pred$row_ids[pred$truth[, 2L] == 0]
  p_cens = pred$clone()$filter(cens_ids)
  expect_true(is.na(p_cens$score(msr("surv.mae"))))
  expect_true(is.na(p_cens$score(msr("surv.mse"))))
  expect_true(is.na(p_cens$score(msr("surv.rmse"))))
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

test_that("calib_index works", {
  m = msr("surv.calib_index")
  expect_equal(m$range, c(0, 1))
  expect_true(m$minimize)
  expect_true(m$param_set$values$na.rm)
  expect_equal(m$param_set$values$method, "ICI") # mean abs diffs
  expect_equal(m$param_set$values$eps, 0.0001)
  res = pred$score(m)
  expect_gt(res, 0)

  # scores for E90 and Emax represent more extreme (larger) differences than the mean
  m2 = msr("surv.calib_index", method = "E90")
  expect_equal(m2$param_set$values$method, "E90")
  res2 = pred$score(m2)
  expect_gt(res2, res)

  m3 = msr("surv.calib_index", method = "Emax")
  expect_equal(m3$param_set$values$method, "Emax")
  expect_gt(pred$score(m3), res2)

  # different time point
  m4 = msr("surv.calib_index", time = 100)
  expect_equal(m4$param_set$values$time, 100)
  expect_false(pred$score(m4) == res)
})

test_that("graf training data for weights", {
  m = msr("surv.graf")
  t = tsk("rats")
  l = lrn("surv.kaplan")
  s1 = l$train(t, 1:50)$predict(t, 51:100)$score(m)
  s2 = l$train(t, 1:50)$predict(t, 51:100)$score(m, task = t, train_set = 1:50)
  expect_false(identical(s1, s2))
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
  m1 = p$score(msr("surv.graf", times = times_flt)) # uses `times_flt`
  m2 = p$score(msr("surv.graf", t_max = t_max)) # 100
  m3 = p$score(msr("surv.graf", t_max = max(times))) # 104
  m4 = p$score(msr("surv.graf", t_max = max(times) + 10)) # 105

  # different time points considered
  expect_true(m0 != m1)
  # same time points are used, and no removal of observations (original Graf score)
  expect_equal(m1, m2)
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

  # mean weighted by the difference between time-points
  s_all = p_cox$score(msr("surv.graf", times = c(68, 92, 102)))
  expect_true(s_all != mean_score)
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

test_that("ERV works", {
  set.seed(1L)
  t = tsk("rats")
  part = partition(t, 0.8)
  l = lrn("surv.kaplan")
  p = l$train(t, part$train)$predict(t, part$test)
  m = msr("surv.graf", ERV = TRUE)
  # ERV = TRUE needs train task
  expect_error(p$score(m))
  # KM is the baseline score, so ERV score = 0
  expect_equal(as.numeric(p$score(m, task = t, train_set = part$train)), 0)

  l = lrn("surv.coxph")
  p = l$train(t, part$train)$predict(t, part$test)
  m = msr("surv.graf", ERV = TRUE)
  # Cox should do a little better than the KM baseline (ERV score > 0)
  expect_gt(as.numeric(p$score(m, task = t, train_set = part$train)), 0)
})

test_that("ERV=TRUE", {
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

  m = msr("surv.rcll")
  expect_true(m$minimize)
  expect_equal(m$range, c(0, Inf))
  KMscore = p$score(m)
  expect_numeric(KMscore)

  status = t$truth()[, 2L]
  row_ids = t$row_ids
  cens_ids = row_ids[status == 0]
  event_ids = row_ids[status == 1]

  # only censored rats in test set
  p = l$predict(t, row_ids = cens_ids)
  score = p$score(m)
  expect_numeric(score)

  # 1 censored test rat
  p = p$filter(row_ids = cens_ids[1L])
  score = p$score(m)
  expect_numeric(score)

  # only dead rats in test set
  p = l$predict(t, row_ids = event_ids)
  score = p$score(m)
  expect_numeric(score)

  # 1 dead rat
  p = p$filter(row_ids = event_ids[1L])
  score = p$score(m)
  expect_numeric(score)

  # Cox is better than baseline (Kaplan-Meier)
  l2 = lrn("surv.coxph")
  p2 = suppressWarnings(l2$train(t)$predict(t))
  expect_lt(p2$score(m), KMscore)

  # Another edge case: some dead rats and 1 only censored
  p3 = p2$filter(row_ids = c(event_ids, cens_ids[1L]))
  score = p3$score(m)
  expect_numeric(score)
})

test_that("logloss works", {
  t = tsk("lung")
  kaplan = lrn("surv.kaplan")
  cox = lrn("surv.coxph")
  p_kaplan = kaplan$train(t)$predict(t)
  p_cox = cox$train(t)$predict(t)

  m = msr("surv.logloss")
  expect_true(m$minimize)
  expect_equal(m$range, c(0, Inf))

  kaplan_score = p_kaplan$score(m)
  expect_numeric(kaplan_score)
  cox_score = p_cox$score(m)
  expect_numeric(cox_score)

  # Cox is a bit better than baseline (Kaplan-Meier) in this dataset
  expect_lt(cox_score, kaplan_score)

  # ERV works as it should
  m = msr("surv.logloss", ERV = TRUE)
  expect_equal(unname(p_kaplan$score(m, task = t, train_set = t$row_ids)), 0)
  expect_gt(unname(p_cox$score(m, task = t, train_set = t$row_ids)), 0)
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
      p$score(m, task = task, train_set = task$row_ids, learner = learner)
    })
  }
})
