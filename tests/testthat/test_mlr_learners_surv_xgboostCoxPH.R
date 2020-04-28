context("surv.xgboost")

test_that("autotest", {
  skip_on_cran()
  set.seed(1)
  learner = mlr_learners$get("surv.xgboost")
  expect_learner(learner)
  result = run_autotest(learner, N = 10, check_replicable = FALSE)
  expect_true(result, info = result$error)
})
