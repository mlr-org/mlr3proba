context("surv.coxph")

test_that("autotest", {
  learner = mlr_learners$get("surv.coxph")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
