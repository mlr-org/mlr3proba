skip_if_not_installed("gss")

test_that("autotest", {
  learner = lrn("dens.spline")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
