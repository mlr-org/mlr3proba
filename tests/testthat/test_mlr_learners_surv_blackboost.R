context("surv.blackboost")

test_that("autotest", {
  skip_on_cran()
  set.seed(1)
  learner = lrn("surv.blackboost")
  expect_learner(learner)
  # weights are fine for all predict types except 'distr'
  result = run_autotest(learner, N = 10, exclude = "weights", check_replicable = FALSE)
  expect_true(result, info = result$error)
})

test_that("ctrlpars", {
  skip_on_cran()
  set.seed(1)
  task = TaskGeneratorSimsurv$new()$generate(10)
  learner = lrn("surv.blackboost", mstop = 99, maxpts = 24000, abseps = 0.1)
  expect_silent(expect_prediction_surv(learner$train(task)$predict(task)))
})
