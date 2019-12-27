context("mlr_measures")

task = TaskGeneratorSimsurv$new()$generate(20)

test_that("mlr_measures", {
  keys = mlr_measures$keys("^surv")

  for (key in keys) {
    if(grepl("TNR|TPR", key))
      m = mlr_measures$get(key, times = 60)
    else
      m = mlr_measures$get(key)

    expect_measure(m)

    perf = mlr_learners$get("surv.coxph")$train(task)$predict(task)$score()
    expect_number(perf, na.ok = "na_score" %in% m$properties)
  }
})

# task = tsk("rats")
learner = lrn("surv.coxph")$train(task)
prediction = learner$predict(task)
times = 60
train_set = 1:175

test_that("unintegrated_prob_losses", {
  msr = lapply(c("surv.logloss","surv.loglossSE"), msr)
  expect_silent(prediction$score(msr))
})

test_that("integrated_prob_losses", {
  probs = paste0("surv.", c("graf","grafSE","intlogloss","intloglossSE"))
  expect_error(lapply(probs, msr, times = 34:37, integrated = FALSE), "non-integrated score")
  expect_silent(prediction$score(lapply(probs, msr, integrated = TRUE)))
  expect_error(prediction$score(lapply(probs, msr, integrated = TRUE, times = c(34:70))), "Times are all")
  expect_silent(prediction$score(lapply(probs, msr, integrated = TRUE, times = c(2:3))))
  expect_silent(prediction$score(lapply(probs, msr, integrated = FALSE, times = 2)))
})

# test_that("AUCs",{
#   aucs = as.data.table(mlr_measures)$key[grepl("AUC", as.data.table(mlr_measures)$key)]
#   expect_error(lapply(aucs, msr, times = 34:37, integrated = FALSE), "non-integrated score")
#   expect_silent(prediction$score(lapply(aucs, msr, integrated = TRUE), task = task, learner = learner, train_set = train_set))
#   expect_silent(prediction$score(lapply(aucs, msr, integrated = TRUE), task = task, learner = learner, train_set = train_set))
# })
#
# test_that("sensspecs",{
#   sensspecs = as.data.table(mlr_measures)$key[grepl("TNR|TPR", as.data.table(mlr_measures)$key)]
#   expect_silent(prediction$score(lapply(sensspecs, msr, integrated = TRUE, times = times), task = task, learner = learner, train_set = train_set))
#   expect_silent(prediction$score(lapply(sensspecs, msr, integrated = TRUE, times = times), task = task, learner = learner, train_set = train_set))
# })
