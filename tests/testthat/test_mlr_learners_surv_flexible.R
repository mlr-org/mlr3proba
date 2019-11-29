context("surv.flexible")

test_that("autotest", {
  set.seed(1)
  learner = lrn("surv.flexible", inits = c(1,2))
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
