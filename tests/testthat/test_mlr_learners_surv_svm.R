context("surv.svm")

test_that("autotest", {
  learner = mlr_learners$get("surv.svm")
  expect_learner(learner)
  expect_true(result, info = result$error)
})
