context("surv.randomForestSRC")

test_that("autotest", {
  learner = mlr_learners$get("surv.randomForestSRC")
  learner$param_set$values = insert_named(learner$param_set$values,
                                          list(importance = "random", na.action = "na.impute"))
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
