context("surv.parametric")

test_that("autotest", {
  learner = mlr_learners$get("surv.parametric")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)

  task = tsk("rats")
  expect_silent(expect_prediction_surv(lrn("surv.parametric", type = "aft")$train(tsk("rats"))$predict(tsk("rats"))))
  expect_silent(expect_prediction_surv(lrn("surv.parametric", type = "ph")$train(tsk("rats"))$predict(tsk("rats"))))
  expect_silent(expect_prediction_surv(lrn("surv.parametric", type = "po")$train(tsk("rats"))$predict(tsk("rats"))))
})
