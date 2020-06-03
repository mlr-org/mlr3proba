context("surv.mboost")

test_that("autotest", {
  learner = mlr_learners$get("surv.mboost")
  learner$param_set$values = insert_named(
    learner$param_set$values,
    list(center = TRUE, baselearner = "bols"))
  expect_learner(learner)
  # weights are fine for all predict types except 'distr'
  result = run_autotest(learner, exclude = "weights", check_replicable = FALSE)
  expect_true(result, info = result$error)
})
