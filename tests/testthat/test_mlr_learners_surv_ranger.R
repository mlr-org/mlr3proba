context("surv.ranger")

test_that("autotest", {
  learner = mlr_learners$get("surv.ranger")
  expect_learner(learner)
  learner$param_set$values = list(importance = "impurity")
  result = run_autotest(learner, check_replicable = FALSE)
  expect_true(result, info = result$error)
  learner = mlr_learners$get("surv.ranger")
  expect_error(learner$importance(), "No model stored")
  expect_error(learner$train(tsk("rats"))$importance(), "No importance stored")
})
