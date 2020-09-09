context("resample")

test_that("resample-step", {
  task = mlr_tasks$get("rats")
  learner = mlr_learners$get("surv.coxph")
  resampling = rsmp("cv", folds = 3)

  rr = resample(task, learner, resampling)
  expect_resample_result(rr)
})
