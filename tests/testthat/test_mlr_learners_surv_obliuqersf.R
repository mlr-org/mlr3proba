context("surv.obliqueRSF")

test_that("autotest", {
  learner = mlr_learners$get("surv.obliqueRSF")
  expect_learner(learner)
  result = run_autotest(learner, exclude = "single", N = 15)
  expect_true(result, info = result$error)
})
