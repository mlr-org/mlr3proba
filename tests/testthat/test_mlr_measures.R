set.seed(1)
task = tsk("rats")$filter(sample(300, 20))
learner = suppressWarnings(lrn("surv.coxph")$train(task))
pred = learner$predict(task)
pred$data$response = 1:20
pred$predict_types = c(pred$predict_types, "response")

test_that("mlr_measures", {
  skip_if_not_installed("survAUC")

  keys = mlr_measures$keys("^surv")

  for (key in keys) {
    if (grepl("TNR|TPR|tpr|tnr", key)) {
      m = msr(key, times = 60)
    } else {
      if (key %in% c("surv.graf", "surv.intlogloss", "surv.schmid", "surv.brier")) {
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

test_that("integrated_prob_losses", {
  set.seed(1)
  t = tsk("rats")$filter(sample(300, 50))
  p = lrn("surv.kaplan")$train(t)$predict(t)
  probs = paste0("surv.", c("graf", "intlogloss", "schmid"))
  lapply(
    probs,
    function(x) expect_error(p$score(msr(x, times = 39:80, integrated = FALSE,
      proper = TRUE)), "scalar numeric")
  )

  prediction$score(msr("surv.intlogloss", integrated = TRUE, proper = TRUE, times = 100:110))
  expect_silent(prediction$score(lapply(probs, msr, integrated = TRUE, proper = TRUE)))
  expect_error(prediction$score(lapply(probs, msr, integrated = TRUE, times = c(34:38), proper = TRUE)), "Requested times")
  expect_silent(prediction$score(lapply(probs, msr, integrated = TRUE, times = c(100:110), proper = TRUE)))
  expect_silent(prediction$score(lapply(probs, msr, integrated = FALSE, times = 80, proper = TRUE)))
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
  expect_equal(m$minimize, FALSE)
  expect_false(m$param_set$values$se)
  expect_equal(m$param_set$values$method, "ratio")
  expect_numeric(pred$score(m))

  m2 = msr("surv.calib_beta", method = "diff") # diff
  expect_equal(m2$range, c(0, Inf))
  expect_equal(m2$minimize, TRUE)
  expect_false(m2$param_set$values$se)
  expect_equal(m2$param_set$values$method, "diff")
  expect_numeric(pred$score(m2))
})

test_that("calib_alpha works", {
  m = msr("surv.calib_alpha") # ratio
  expect_equal(m$range, c(-Inf, Inf))
  expect_equal(m$minimize, FALSE)
  expect_false(m$param_set$values$se)
  expect_false(is.finite(m$param_set$values$truncate))
  expect_equal(m$param_set$values$method, "ratio")
  expect_numeric(pred$score(m))

  m2 = msr("surv.calib_alpha", method = "diff") # diff
  expect_equal(m2$range, c(0, Inf))
  expect_equal(m2$minimize, TRUE)
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
  set.seed(1)
  m1 = msr("surv.graf", proper = TRUE, method = 1)
  m2 = suppressWarnings(msr("surv.graf", proper = FALSE, method = 1))
  l = lrn("surv.kaplan")
  p = l$train(tsk("rats"), row_ids = sample(300, 50))$
    predict(tsk("rats"), row_ids = sample(300, 50))
  s1 = p$score(m1)
  s2 = p$score(m2)
  expect_gt(s2, s1)
})

test_that("t_max, p_max", {
  set.seed(1)
  t = tsk("rats")$filter(sample(1:300, 50))
  p = lrn("surv.kaplan")$train(t)$predict(t)

  expect_error(p$score(msr("surv.graf", integrated = FALSE, times = 1:2)))
  expect_error(p$score(msr("surv.graf", integrated = FALSE)))
  expect_error(p$score(msr("surv.graf", times = 1:2, t_max = 3)))

  m1 = p$score(msr("surv.graf", times = seq(100)))
  m2 = p$score(msr("surv.graf", t_max = 100))
  expect_equal(m1, m2)

  s = t$kaplan()

  t_max = s$time[which(1 - s$n.risk / s$n > 0.3)[1]]

  m1 = p$score(msr("surv.graf", t_max = t_max))
  m2 = p$score(msr("surv.graf", p_max = 0.3))
  expect_equal(m1, m2)
})


test_that("ERV works as expected", {
  set.seed(1)
  t = tsk("rats")$filter(sample(1:300, 50))
  l = lrn("surv.kaplan")
  p = l$train(t)$predict(t)
  m = msr("surv.graf", ERV = TRUE)
  expect_equal(as.numeric(p$score(m, task = t, train_set = t$row_ids)), 0)
  expect_equal(as.numeric(resample(t, l, rsmp("holdout"))$aggregate(m)), 0)

  set.seed(1)
  t = tsk("rats")$filter(sample(1:300, 100))
  l = lrn("surv.coxph")
  p = suppressWarnings(l$train(t)$predict(t))
  m = msr("surv.graf", ERV = TRUE)
  expect_gt(as.numeric(p$score(m, task = t, train_set = t$row_ids)), 0)
  expect_gt(suppressWarnings(as.numeric(resample(t, l, rsmp("holdout"))$
    aggregate(m))), 0)

  set.seed(1)
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
  set.seed(1)
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

  status  = t$truth()[,2]
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
  p = p$filter(row_ids = cens_ids[1])
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
  p = p$filter(row_ids = event_ids[1])
  score = p$score(m)
  expect_numeric(score)
  p2 = p$clone() # test score via distribution
  p2$data$distr = p2$distr[1] # WeightDisc
  score2 = p2$score(m)
  expect_equal(score, score2)

  # Cox is better than baseline (Kaplan-Meier)
  l2 = lrn("surv.coxph")
  p2 = suppressWarnings(l2$train(t)$predict(t))
  expect_true(p2$score(m) < KMscore)

  # Another edge case: some dead rats and 1 only censored
  p3 = p2$filter(row_ids = c(event_ids, cens_ids[1]))
  score = p3$score(m)
  expect_numeric(score)
  p3$data$distr = p3$distr
  score2 = p3$score(m)
  expect_equal(score, score2)
})

test_that("dcal works", {
  set.seed(1)
  t = tsk("rats")$filter(sample(1:300, 50))
  l = lrn("surv.coxph")
  p = suppressWarnings(l$train(t)$predict(t))
  p2 = p$clone()
  p2$data$distr = p2$distr # hack: test score via distribution
  m = msr("surv.dcalib", truncate = 20)
  expect_true(m$minimize)
  expect_equal(m$range, c(0, Inf))
  expect_equal(m$param_set$values$B, 10)
  expect_equal(m$param_set$values$chisq, FALSE)
  expect_equal(m$param_set$values$truncate, 20)
  KMscore = p$score(m)
  expect_numeric(KMscore)
  KMscore2 = p2$score(m)
  expect_equal(KMscore, KMscore2)

  status  = t$truth()[,2]
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
  p = p$filter(row_ids = cens_ids[1])
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
  p = p$filter(row_ids = event_ids[1])
  score = p$score(m)
  expect_numeric(score)
  p2 = p$clone() # test score via distribution
  p2$data$distr = p2$distr[1] # WeightDisc
  score2 = p2$score(m)
  expect_equal(score, score2)

  # Another edge case: some dead rats and 1 only censored
  p = l$predict(t, row_ids = c(event_ids, cens_ids[1]))
  score = p$score(m)
  expect_numeric(score)
  p$data$distr = p$distr
  score2 = p$score(m)
  expect_equal(score, score2)
  expect_true(score > 10)

  score3 = p$score(msr("surv.dcalib", truncate = 10))
  expect_equal(unname(score3), 10)
  score4 = p$score(msr("surv.dcalib", truncate = 5))
  expect_equal(unname(score4), 5)
  score5 = p$score(msr("surv.dcalib", truncate = Inf, B = 20)) # B affects truncate
  expect_true(score5 > score)
})

test_that("logloss works", {
  set.seed(1)
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

  status  = t$truth()[,2]
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
  p = p$filter(row_ids = cens_ids[1])
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
  p = p$filter(row_ids = event_ids[1])
  score = p$score(m)
  expect_numeric(score)
  expect_equal(score, p$score(m2))
  p2 = p$clone() # test score via distribution
  p2$data$distr = p2$distr[1] # WeightDisc
  score2 = p2$score(m)
  expect_equal(score, score2)

  # Cox is better than baseline (Kaplan-Meier)
  l2 = lrn("surv.coxph")
  p2 = suppressWarnings(l2$train(t)$predict(t))
  expect_true(p2$score(m) < KMscore)

  # Another edge case: some dead rats and 1 only censored
  p3 = p2$clone()$filter(row_ids = c(event_ids, cens_ids[1]))
  score = p3$score(m)
  expect_numeric(score)
  expect_true(score != p3$score(m2)) # since dead rats are removed
  p3$data$distr = p3$distr
  score2 = p3$score(m)
  expect_equal(score, score2)

  # Another edge case: some censored rats and 1 only dead
  p4 = p2$clone()$filter(row_ids = c(cens_ids, event_ids[1]))
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
  expect_class(p$data$distr, "matrix")
  expect_class(p$distr, "Matdist")

  # hack: substitute with 3d survival array
  p = reshape_distr_to_3d(p)
  expect_class(p$data$distr, "array")
  expect_class(p$distr, "Arrdist") # `distr6` interface class changed

  distr_msrs = msrs(as.data.table(mlr_measures)[
    predict_type == 'distr' & startsWith(key, 'surv')
  ]$key)

  for (m in distr_msrs) {
    expect_numeric({
      p$score(m, task = task, train_set = seq(task$nrow), learner = learner)
    })
  }
})
