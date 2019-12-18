context("surv.randomForestSRC")

test_that("autotest", {
  learner = mlr_learners$get("surv.randomForestSRC")
  learner$param_set$values = insert_named(learner$param_set$values,
                                          list(importance = "random", na.action = "na.impute"))
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})

test_that("importance/selected",{
  set.seed(1)
  task = TaskGeneratorSimsurv$new()$generate(20)
  learner = lrn("surv.randomForestSRC", estimator = "kaplan")
  expect_error(learner$importance(), "No model stored")
  expect_error(learner$selected_features(), "No model stored")
  learner$train(task)
  expect_error(learner$importance(), "Importance not stored")
  expect_error(learner$selected_features(), "Variables used not stored")
  learner = lrn("surv.randomForestSRC", estimator = "nelson", var.used = "all.trees", importance = "random")$train(task)
  expect_silent(learner$selected_features())
  expect_silent(learner$importance())
})
