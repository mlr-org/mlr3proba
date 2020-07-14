context("mlr_measures")
library(mlr3learners.survival)
set.seed(1)
task = TaskGeneratorSimsurv$new()$generate(20)
learner = lrn("surv.coxph")$train(task)
pred = learner$predict(task)
pred_resp = LearnerSurvParametric$new()$train(task)$predict(task)

test_that("mlr_measures", {
  keys = mlr_measures$keys("^surv")

  for (key in keys) {
    if (grepl("TNR|TPR", key)) {
      m = msr(key, times = 60)
    } else {
      m = msr(key)
    }

    expect_measure(m)

    if (m$predict_type == "response") {
      perf = pred_resp$score(m, task = task, train_set = seq(task$nrow), learner = learner)
    } else {
      perf = pred$score(m, task = task, train_set = seq(task$nrow), learner = learner)
    }

    expect_number(perf, na.ok = "na_score" %in% m$properties)

    if (key %in% c("surv.calib_alpha", "surv.calib_beta")) {
      m = msr(key, se = TRUE)
      perf = pred$score(m, task = task, train_set = seq(task$nrow), learner = learner)
      expect_number(perf, na.ok = "na_score" %in% m$properties)
    }
  }
})

# task = tsk("rats")
learner = lrn("surv.coxph")$train(task)
prediction = learner$predict(task)

test_that("unintegrated_prob_losses", {
  msr = lapply(c("surv.logloss", "surv.loglossSE"), msr)
  expect_silent(prediction$score(msr))
})

test_that("integrated_prob_losses", {
  probs = paste0("surv.", c("graf", "grafSE", "intlogloss", "intloglossSE", "schmid", "schmid_se"))
  expect_error(lapply(probs, msr, times = 34:37, integrated = FALSE), "non-integrated score")
  expect_silent(prediction$score(lapply(probs, msr, integrated = TRUE)))
  expect_error(prediction$score(lapply(probs, msr, integrated = TRUE, times = c(34:70))),
               "Times are all")
  expect_silent(prediction$score(lapply(probs, msr, integrated = TRUE, times = c(2:3))))
  expect_silent(prediction$score(lapply(probs, msr, integrated = FALSE, times = 2)))
})

times = 60
train_set = 1:20

test_that("AUCs",{
  aucs = as.data.table(mlr_measures)$key[grepl("AUC", as.data.table(mlr_measures)$key)]
  expect_error(lapply(aucs, msr, times = 34:37, integrated = FALSE), "non-integrated score")
  expect_silent(prediction$score(lapply(aucs, msr, integrated = TRUE), task = task,
  learner = learner, train_set = train_set))
  expect_silent(prediction$score(lapply(aucs, msr, integrated = TRUE), task = task,
  learner = learner, train_set = train_set))
})

test_that("sensspecs",{
  sensspecs = as.data.table(mlr_measures)$key[grepl("TNR|TPR", as.data.table(mlr_measures)$key)]
  expect_silent(prediction$score(lapply(sensspecs, msr, integrated = TRUE, times = times),
   task = task, learner = learner, train_set = train_set))
  expect_silent(prediction$score(lapply(sensspecs, msr, integrated = TRUE, times = times),
  task = task, learner = learner, train_set = train_set))
})
