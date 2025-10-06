skip_if_not_installed("logspline")

test_that("autotest", {
  set.seed(1L)
  learner = lrn("dens.logspline")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
