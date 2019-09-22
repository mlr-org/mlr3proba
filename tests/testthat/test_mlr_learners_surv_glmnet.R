context("surv.glmnet")

test_that("autotest", {
  learner = mlr_learners$get("surv.glmnet")
  expect_learner(learner)
  result = run_autotest(learner, exclude = "feat_single")
  expect_true(result, info = result$error)
})
