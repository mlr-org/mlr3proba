context("PipeOpCrankCompositor")

test_that("PipeOpCrankCompositor - basic properties", {
  expect_pipeop(PipeOpCrankCompositor$new())
})

test_that("PipeOpCrankCompositor - assertions", {
  expect_error(crankcompositor(lrn("surv.glmnet")), "Assertion on 'distr'")
  expect_error(po("crankcompose")$predict(
    list(lrn("surv.glmnet")$train(tsk("rats"))$predict(tsk("rats")))), "Assertion on 'distr'")
})

test_that("PipeOpCrankCompositor - estimate", {
  gr = crankcompositor(lrn("surv.coxph"), method = "median")
  task = tsk("rats")
  expect_equal(gr$train(task), list(crankcompose.output = NULL))
  p = gr$predict(task)$crankcompose.output
  expect_prediction_surv(p)
  expect_true("crank" %in% p$predict_types)
})

