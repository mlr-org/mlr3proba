test_that("autotest", {
  learner = mlr_learners$get("surv.rpart")
  expect_learner(learner)
  result = run_autotest(learner, check_replicable = FALSE)
  expect_true(result, info = result$error)
})


test_that("importance/selected", {
  learner = lrn("surv.rpart")
  expect_error(learner$importance(), "No model stored")
  expect_error(learner$selected_features(), "No model stored")
  learner$train(tsk("rats"))
  expect_silent(learner$selected_features())
  expect_silent(learner$importance())
})
