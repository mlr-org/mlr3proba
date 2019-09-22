context("resampling")

test_that("resampling works", {
  task = tsk("lung")
  learner = lrn("surv.rpart")
  rr = resample(task, learner, mlr_resamplings$get("cv"))
  expect_resample_result(rr)
})
