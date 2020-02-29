context("dens.penLP")

test_that("autotest", {
  set.seed(1)
  learner = lrn("dens.penLP")
  expect_learner(learner)
  result = run_autotest(learner, N= 50)
  expect_true(result, info = result$error)
})
