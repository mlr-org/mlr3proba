context("surv.ranger")

test_that("autotest", {
  learner = mlr_learners$get("surv.ranger")
  expect_learner(learner)
  learner$param_set$values = list(importance = "impurity")
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
