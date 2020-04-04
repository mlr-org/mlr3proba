context("surv.penalized")

library(penalized)

test_that("autotest", {
  learner = mlr_learners$get("surv.penalized")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})

test_that("missing",{
  learner = lrn("surv.penalized")
  task = tsk("lung")
  expect_error(learner$train(task), "Missing data")
})

test_that("unpenalized",{
  task = tgen("simsurv")$generate(200)
  learner = lrn("surv.penalized", unpenalized = c("height"))
  learner$train(task)
  expect_equal(names(learner$model@penalized), c("treatment", "weight"))
  expect_equal(names(learner$model@unpenalized), c("height"))
  expect_prediction_surv(learner$predict(task))
})
