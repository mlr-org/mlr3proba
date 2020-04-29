context("surv.glmboost")

test_that("autotest", {
  learner = mlr_learners$get("surv.glmboost")
  expect_learner(learner)
  # weights are fine for all predict types except 'distr'
  result = run_autotest(learner, exclude = "weights", check_replicable = FALSE)
  expect_true(result, info = result$error)
})
