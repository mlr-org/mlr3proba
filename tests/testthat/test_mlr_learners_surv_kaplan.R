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
  expect_prediction_surv(learner$predict(task, task$row_ids[1]))
})

test_that("importance/selected", {
  learner = lrn("surv.kaplan")
  expect_error(learner$importance(), "No model stored")
  expect_error(learner$selected_features(), "No model stored")

  task = tsk("rats")
  learner$train(task)
  expect_character(learner$selected_features(), len = 0)
  expect_named(learner$importance(), expected = task$feature_names)
  expect_true(all(learner$importance() == 0))
})
