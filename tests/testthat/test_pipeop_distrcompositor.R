context("PipeOpDistrCompositor")

test_that("PipeOpDistrCompositor - basic properties", {
  expect_pipeop(PipeOpDistrCompositor$new())
  expect_pipeop(PipeOpDistrCompositor$new(param_vals = list()))
})

test_that("PipeOpDistrCompositor - assertions", {
  task = tsk("rats")
  base = lrn("surv.glmnet")$train(task)$predict(task)
  pred = lrn("surv.coxph")$train(task)$predict(task)
  pod = po("distrcompose", param_vals = list(form = "aft", overwrite = TRUE))
  expect_error(pod$predict(list(base = base, pred = pred)), "Assertion on 'distr'")
  expect_silent(pod$predict(list(base = lrn("surv.kaplan")$train(task)$predict(task),
                                pred = lrn("surv.nelson")$train(task)$predict(task))))
})

test_that("PipeOpDistrCompositor - overwrite = FALSE", {
  gr = distrcompositor(lrn("surv.coxph"), overwrite = FALSE)
  task = tsk("rats")
  expect_silent(gr$train(task))
  expect_equal(gr$predict(task)$distr,
               lrn("surv.coxph")$train(task)$predict(task)$distr)
})

test_that("PipeOpDistrCompositor - overwrite = TRUE", {
  gr = distrcompositor(lrn("surv.coxph"), overwrite = TRUE, form = "ph")
  task = tsk("rats")
  expect_silent(gr$train(task))
  p = gr$predict(task)
  expect_prediction_surv(p)
  expect_true("distr" %in% p$predict_types)

  gr = distrcompositor(lrn("surv.coxph"), overwrite = TRUE, form = "po")
  task = tsk("rats")
  expect_silent(expect_prediction_surv(gr$train(task)$predict(task)))
})


test_that("no params", {
  task = tsk("rats")
  base = lrn("surv.glmnet")$train(task)$predict(task)
  pred = lrn("surv.coxph")$train(task)$predict(task)
  pod = po("distrcompose", param_vals = list())
  expect_silent(pod$predict(list(base = base, pred = pred)))
})


