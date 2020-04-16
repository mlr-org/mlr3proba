context("PipeOpCrankCompositor")

test_that("PipeOpCrankCompositor - basic properties", {
  expect_pipeop(PipeOpCrankCompositor$new())
  expect_equal(PipeOpCrankCompositor$new()$param_set$values$method, "mean")
})

task = tgen("simsurv")$generate(20)

# test_that("PipeOpCrankCompositor - assertions", {
#   expect_error(crankcompositor(lrn("surv.svm")), "Assertion on 'distr'")
#   expect_error(po("crankcompose")$predict(
#     list(lrn("surv.svm")$train(task)$predict(task))), "Assertion on 'distr'")
# })

test_that("PipeOpCrankCompositor - estimate", {
  gr = crankcompositor(lrn("surv.coxph"), method = "mode")
  expect_silent(gr$train(task))
  p = gr$predict(task)
  expect_prediction_surv(p)
  expect_true("crank" %in% p$predict_types)
})

test_that("no params",{
  po = PipeOpCrankCompositor$new(param_vals = list())
  p = po$predict(
    list(lrn("surv.kaplan")$train(task)$predict(task)))$output
  expect_prediction_surv(p)
  expect_equal(p$lp, numeric(0))
})
