test_that("autotest", {
  learner = mlr_learners$get("surv.kaplan")
  expect_learner(learner)
  result = run_autotest(learner, check_replicable = FALSE, N = 10)
  expect_true(result, info = result$error)
})
