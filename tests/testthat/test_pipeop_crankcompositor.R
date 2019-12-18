context("PipeOpCrankCompositor")

test_that("PipeOpCrankCompositor - basic properties", {
  expect_pipeop(PipeOpCrankCompositor$new())
  expect_equal(PipeOpCrankCompositor$new()$param_set$values$method, "mean")
})

test_that("PipeOpCrankCompositor - assertions", {
  expect_error(crankcompositor(lrn("surv.glmnet")), "Assertion on 'distr'")
  expect_error(po("crankcompose")$predict(
    list(lrn("surv.glmnet")$train(tsk("rats"))$predict(tsk("rats")))), "Assertion on 'distr'")
})

test_that("PipeOpCrankCompositor - estimate", {
  gr = crankcompositor(lrn("surv.coxph"), method = "median")
  task = tsk("rats")
  expect_silent(gr$train(task))
  p = gr$predict(task)
  expect_prediction_surv(p)
  expect_true("crank" %in% p$predict_types)
})

test_that("no params",{
  po = PipeOpCrankCompositor$new(param_vals = list())
  p = po$predict(
    list(lrn("surv.kaplan")$train(tsk("rats"))$predict(tsk("rats"))))$output
  expect_prediction_surv(p)
  expect_equal(p$lp, numeric(0))
})
