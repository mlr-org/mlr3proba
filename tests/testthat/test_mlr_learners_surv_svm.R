context("surv.svm")

test_that("autotest", {
  makediff1 = survivalsvm::makediff1
  makediff2 = survivalsvm::makediff2
  makediff3 = survivalsvm::makediff3
  learner = mlr_learners$get("surv.svm")
  expect_learner(learner)
  result = run_autotest(learner,exclude = "feat_single")
  expect_true(result, info = result$error)
})
