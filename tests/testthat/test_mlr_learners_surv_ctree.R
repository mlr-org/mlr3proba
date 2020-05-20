context("surv.CTree")

test_that("autotest", {
  learner = mlr_learners$get("surv.ctree")
  expect_learner(learner)
  result = run_autotest(learner, N = 15)
  expect_true(result, info = result$error)
})
