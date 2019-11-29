context("surv.glmboost")

test_that("autotest", {
  learner = mlr_learners$get("surv.glmboost")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
