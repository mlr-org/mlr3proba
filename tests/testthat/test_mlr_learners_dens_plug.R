skip_if_not_installed("plugdensity")

test_that("autotest", {
  learner = lrn("dens.plug")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
