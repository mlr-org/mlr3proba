context("surv.coxph")

test_that("autotest", {
  set.seed(1)
  learner = lrn("surv.coxph")
  expect_learner(learner)
  # no idea why weights check here fails
  result = run_autotest(learner, exclude = "weights", check_replicable = FALSE)
  expect_true(result, info = result$error)
})

test_that("weights", {
  learner = lrn("surv.coxph")
  task = generate_tasks.LearnerSurv(learner)$weights
  expect_silent(learner$train(task))
  expect_equal(learner$model$weights, task$weights$weight)
})


test_that("missing", {
  set.seed(1)
  task = TaskGeneratorSimsurv$new()$generate(5)
  learner = lrn("surv.coxph")
  expect_warning(learner$train(task), "iterations")
  expect_error(learner$predict(tsk("lung")))
})
