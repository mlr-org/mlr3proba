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
                                         proper = TRUE)),
                            "scalar numeric")
  )

  prediction$score(msr("surv.intlogloss", integrated = TRUE, proper = TRUE, times = 100:110))
  expect_silent(prediction$score(lapply(probs, msr, integrated = TRUE, proper = TRUE)))
  expect_error(prediction$score(lapply(probs, msr, integrated = TRUE, times = c(34:38), proper = TRUE)), "Requested times")
  expect_silent(prediction$score(lapply(probs, msr, integrated = TRUE, times = c(100:110), proper = TRUE)))
  expect_silent(prediction$score(lapply(probs, msr, integrated = FALSE, times = 80, proper = TRUE)))
})

test_that("dcalib", {
  expect_equal(
    pchisq(prediction$score(msr("surv.dcalib", B = 14)), df = 13, lower.tail = FALSE),
    suppressWarnings(prediction$score(msr("surv.dcalib", B = 14, chisq = TRUE)))
  )
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
