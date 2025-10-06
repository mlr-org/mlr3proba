skip_if_not_installed("sm")

test_that("autotest", {
  learner = lrn("dens.nonpar")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
