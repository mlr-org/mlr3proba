context("surv.mboost")

test_that("autotest", {
  learner = mlr_learners$get("surv.mboost")
  expect_learner(learner)
  # result = run_autotest(learner, N = 8000, exclude = "all|single")
  # expect_true(result, info = result$error)
})
