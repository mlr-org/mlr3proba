skip_if_not_installed("np")

test_that("autotest", {
  learner = lrn("dens.mixed")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
