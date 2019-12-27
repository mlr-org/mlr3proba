context("surv.cvglmnet")

test_that("autotest", {
  skip_on_cran()
  learner = mlr_learners$get("surv.cvglmnet")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})

test_that("ctrl.pars",{
  skip_on_cran()
  set.seed(1)
  task = tgen("simsurv")$generate(20)
  learner = mlr_learners$get("surv.cvglmnet")
  learner$param_set$values = list(mxit = 90, s = 0.1)
  expect_silent(expect_prediction_surv(learner$train(task)$predict(task)))
})
