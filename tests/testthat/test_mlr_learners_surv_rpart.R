test_that("autotest", {
  learner = mlr_learners$get("surv.rpart")
  expect_learner(learner)
  ## excluding because I don't know what it does or how to test it..
  result = run_autotest(learner, exclude = "utf8_feature_names",
                        check_replicable = FALSE)
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

test_that("keep_model", {
  learner = lrn("surv.rpart", keep_model = TRUE)
  learner$train(tsk("rats"))
  expect_false(is.null(learner$model$model))
})
