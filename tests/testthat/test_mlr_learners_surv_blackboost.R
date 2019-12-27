context("surv.blackboost")

test_that("autotest", {
  skip_on_cran()
  set.seed(1)
  learner = mlr_learners$get("surv.blackboost")
  expect_learner(learner)
  result = run_autotest(learner, N = 10)
  expect_true(result, info = result$error)
})

test_that("ctrlpars",{
  skip_on_cran()
  set.seed(1)
  task = TaskGeneratorSimsurv$new()$generate(10)
  learner = lrn("surv.blackboost", mstop = 99, maxpts = 24000, abseps = 0.1)
  expect_silent(expect_prediction_surv(learner$train(task)$predict(task)))
})
