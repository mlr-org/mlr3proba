test_that("autotest", {
  set.seed(1L)
  learner = mlr_learners$get("surv.kaplan")
  expect_learner(learner)
  # sanity requires c > 0.5, kaplan always = 0.5
  result = run_autotest(learner, check_replicable = FALSE, N = 10, exclude = "sanity")
  expect_true(result, info = result$error)
})

test_that("single prediction", {
  set.seed(1L)
  task = tsk("rats")$filter(sample(300, 20))
  learner = mlr_learners$get("surv.kaplan")
  learner$train(task)
  expect_prediction_surv(learner$predict(task, 1))
})
