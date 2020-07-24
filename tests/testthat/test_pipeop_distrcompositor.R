context("PipeOpDistrCompositor")

test_that("PipeOpDistrCompositor - basic properties", {
  expect_pipeop(PipeOpDistrCompositor$new())
})

task = tgen("simsurv")$generate(10)

test_that("PipeOpDistrCompositor - overwrite = FALSE", {
  gr = distrcompositor(lrn("surv.kaplan", id = "k2"), overwrite = FALSE)
  expect_silent(gr$train(task))
  expect_equal(
    gr$predict(task)$distr,
    lrn("surv.kaplan", id = "k2")$train(task)$predict(task)$distr)
})

test_that("PipeOpDistrCompositor - overwrite = TRUE", {
  gr = distrcompositor(lrn("surv.kaplan", id = "k2"), overwrite = TRUE, form = "ph")
  expect_silent(gr$train(task))
  p = gr$predict(task)
  expect_prediction_surv(p)
  expect_true("distr" %in% p$predict_types)
})


test_that("no params", {
  base = lrn("surv.kaplan")$train(task)$predict(task)
  pred = lrn("surv.kaplan", id = "k2")$train(task)$predict(task)
  pod = po("distrcompose", param_vals = list())
  expect_silent(pod$predict(list(base = base, pred = pred)))
})
