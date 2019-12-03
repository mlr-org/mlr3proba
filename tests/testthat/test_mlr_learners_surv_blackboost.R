context("surv.blackboost")

test_that("autotest", {
  learner = mlr_learners$get("surv.blackboost")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})

test_that("ctrlpars",{
  learner = lrn("surv.blackboost", mstop = 99, maxpts = 24000, abseps = 0.1)
  expect_silent(expect_prediction_surv(learner$train(tsk("rats"))$predict(tsk("rats"))))
})
