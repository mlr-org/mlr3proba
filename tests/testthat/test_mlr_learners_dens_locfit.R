skip_if_not_installed("locfit")

test_that("autotest", {
  learner = lrn("dens.locfit")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
