context("pecs")

test_that("pecs.list", {
  task = tsk("rats")
  learns = lrns(c("surv.kaplan", "surv.coxph", "surv.ranger"))
  expect_error(pecs(learns), "trained learners")
  lapply(learns, function(x) x$train(task))
  expect_error(pecs(learns, meas = "sdsd"))
  expect_silent(pecs(learns, task = task, times = c(20, 90), n = 10))
  expect_silent(pecs(learns, task = task, n = 10))
  expect_silent(pecs(learns, task = task, meas = "logloss"))
  expect_error(pecs(learns, times = 1))
  expect_error(pecs(learns, times = -10:-5))
})

test_that("pecs.PredictionSurv", {
  p = lrn("surv.coxph")$train(task)$predict(task)
  expect_silent(pecs(p))
})
