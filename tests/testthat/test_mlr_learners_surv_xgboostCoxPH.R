context("surv.blackboost")

test_that("autotest", {
  skip_on_cran()
  set.seed(1)
  learner = mlr_learners$get("surv.XGBoostCoxPH")
  expect_learner(learner)
  result = run_autotest(learner, N = 10)
  expect_true(result, info = result$error)
})
