context("surv.mboost")

test_that("autotest", {
  learner = mlr_learners$get("surv.mboost")
  learner$param_set$values = mlr3misc::insert_named(learner$param_set$values,
                                                    list(center = TRUE, baselearner = "bols"))
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})
