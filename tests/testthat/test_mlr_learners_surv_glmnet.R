context("surv.glmnet")

test_that("autotest", {
  learner = mlr_learners$get("surv.glmnet")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})

test_that("ctrl.pars",{
  task = tsk("rats")
  learner = mlr_learners$get("surv.glmnet")
  learner$param_set$values$mxit = 90
  expect_silent(expect_prediction_surv(learner$train(task)$predict(task)))
})
