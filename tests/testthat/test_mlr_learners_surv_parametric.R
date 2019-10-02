context("surv.parametric")

test_that("autotest", {
  learner = mlr_learners$get("surv.parametric")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
