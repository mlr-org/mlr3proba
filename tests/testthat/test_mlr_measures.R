set.seed(1)
task = TaskGeneratorSimsurv$new()$generate(20)
learner = lrn("surv.coxph")$train(task)
pred = learner$predict(task)
pred$data$response = 1:20
pred$predict_types = c(pred$predict_types, "response")

test_that("mlr_measures", {
  keys = mlr_measures$keys("^surv")

  for (key in keys) {
    if (grepl("TNR|TPR", key)) {
      m = msr(key, times = 60)
    } else {
      if (key %in% c("surv.graf", "surv.intlogloss", "surv.schmid", "surv.brier")) {
        m = msr(key, proper = TRUE)
      } else {
        m = msr(key)
      }
    }

    expect_measure(m)

    perf = pred$score(m, task = task, train_set = seq(task$nrow), learner = learner)
    expect_number(perf, na.ok = "na_score" %in% m$properties)

    if (key %in% paste0("surv.", c("schmid", "graf", "intlogloss", "logloss", "mae", "mse",
                                   "rmse", "calib_alpha", "calib_beta"))) {
      m = suppressWarnings(msr(key, se = TRUE))
      perf = pred$score(m, task = task, train_set = seq(task$nrow), learner = learner)
      expect_number(perf, na.ok = "na_score" %in% m$properties)
    }
  }
})

test_that("print", {
  expect_output(msr("surv.cindex")$print(), "Score")
  expect_output(msr("surv.graf", proper = TRUE)$print(), "Score")
  expect_output(msr("surv.graf", se = TRUE, proper = TRUE)$print(), "Standard Error")
})

# task = tsk("rats")
learner = lrn("surv.coxph")$train(task)
prediction = learner$predict(task)

test_that("unintegrated_prob_losses", {
  msr = msr("surv.logloss")
  expect_silent(prediction$score(msr))
})

test_that("integrated_prob_losses", {
  probs = paste0("surv.", c("graf", "intlogloss", "schmid"))
  expect_error(lapply(probs, msr, times = 34:37, integrated = FALSE, proper = TRUE),
                "non-integrated score")
  expect_silent(prediction$score(lapply(probs, msr, integrated = TRUE, proper = TRUE)))
  expect_error(prediction$score(lapply(probs, msr, integrated = TRUE, times = c(34:70),
               proper = TRUE)), "Requested times")
  expect_silent(prediction$score(lapply(probs, msr, integrated = TRUE, times = c(2:3),
                proper = TRUE)))
  expect_silent(prediction$score(lapply(probs, msr, integrated = FALSE, times = 2, proper = TRUE)))
})


times = 60
train_set = 1:20

test_that("AUCs",{
  aucs =  mlr_measures$keys("^surv.[a-zA-Z]*_auc")
  expect_error(lapply(aucs, msr, times = 34:37, integrated = FALSE), "non-integrated score")
  expect_silent(prediction$score(lapply(aucs, msr, integrated = TRUE), task = task,
  learner = learner, train_set = train_set))
  expect_silent(prediction$score(lapply(aucs, msr, integrated = TRUE), task = task,
  learner = learner, train_set = train_set))
})

test_that("sensspecs",{
  sensspecs = mlr_measures$keys("^surv.[a-zA-Z]*_tpr|^surv.[a-zA-Z]*_tnr")
  expect_silent(prediction$score(lapply(sensspecs, msr, integrated = TRUE, times = times),
   task = task, learner = learner, train_set = train_set))
  expect_silent(prediction$score(lapply(sensspecs, msr, integrated = TRUE, times = times),
  task = task, learner = learner, train_set = train_set))
})

test_that("dcalib", {
  expect_equal(
    pchisq(prediction$score(msr("surv.dcalib", B = 14)), df = 13, lower.tail = FALSE),
    suppressWarnings(prediction$score(msr("surv.dcalib", B = 14, chisq = TRUE)))
  )
})
