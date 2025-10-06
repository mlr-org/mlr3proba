skip_if_not_installed("ks")

test_that("autotest", {
  learner = lrn("dens.kde_ks")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
