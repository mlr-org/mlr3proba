context("surv.coxph")

test_that("autotest", {
  set.seed(1)
  learner = lrn("surv.coxph")
  expect_learner(learner)
  # no idea why weights check here fails
  result = run_autotest(learner, exclude = "weights")
  expect_true(result, info = result$error)
})

test_that("weights",{
  learner = lrn("surv.coxph")
  task = generate_tasks.LearnerSurv(learner)$weights
  expect_silent(learner$train(task))
  expect_equal(learner$model$weights, task$weights$weight)
})


test_that("missing",{
  task = TaskGeneratorSimsurv$new()$generate(50)
  learner = lrn("surv.coxph")
  learner$train(task)
  expect_error(learner$predict(tsk("lung")))
})
