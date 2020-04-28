context("dens.logspline")

test_that("autotest", {
  set.seed(1)
  learner = lrn("dens.logspline")
  expect_learner(learner)
  result = run_autotest(learner, check_replicable = FALSE)
  expect_true(result, info = result$error)
})
