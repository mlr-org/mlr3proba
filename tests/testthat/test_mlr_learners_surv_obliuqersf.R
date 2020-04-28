context("surv.obliqueRSF")

test_that("autotest", {
  learner = mlr_learners$get("surv.obliqueRSF")
  expect_learner(learner)
  result = run_autotest(learner, exclude = "single", N = 15, check_replicable = FALSE)
  expect_true(result, info = result$error)
})
