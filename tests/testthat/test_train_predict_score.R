context("resampling")

test_that("train, predict, score", {
  task = tsk("lung")
  learner = lrn("surv.rpart")

  perf = learner$train(task)$predict(task)$score()
  expect_number(perf)
})
