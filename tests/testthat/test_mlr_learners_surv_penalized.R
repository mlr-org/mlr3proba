context("surv.penalized")

test_that("autotest", {
  learner = mlr_learners$get("surv.penalized")
  library(penalized)
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
