test_that("PipeOpCrankCompositor - basic properties", {
  expect_pipeop(PipeOpCrankCompositor$new())
  expect_equal(PipeOpCrankCompositor$new()$param_set$values$method, "mean")
})

task = tgen("simsurv")$generate(20)

test_that("PipeOpCrankCompositor - estimate", {
  gr = mlr3pipelines::ppl("crankcompositor", lrn("surv.coxph"), method = "mode", which = 1)
  expect_silent(gr$train(task))
  p = gr$predict(task)[[1]]
  expect_prediction_surv(p)
  expect_true("crank" %in% p$predict_types)
})

test_that("no params", {
  po = PipeOpCrankCompositor$new(param_vals = list())
  p = po$predict(
    list(lrn("surv.kaplan")$train(task)$predict(task)))$output
  expect_prediction_surv(p)
  expect_equal(p$lp, rep(NA_real_, 20))
})

test_that("response", {
  po = PipeOpCrankCompositor$new(param_vals = list(response = TRUE))
  p = po$predict(
    list(lrn("surv.kaplan")$train(task)$predict(task)))$output
  expect_equal(p$response, p$crank)

  p = pipeline_crankcompositor(lrn("surv.coxph"), response = TRUE,
                               graph_learner = TRUE)$train(task)$predict(task)
  expect_equal(p$response, p$crank)
})
