context("surv.svm")

test_that("autotest", {
  learner = mlr_learners$get("surv.svm")
  expect_learner(learner)
  result = run_autotest(learner,exclude = "feat_single")
  expect_true(result, info = result$error)
})
