test_that("PipeOpDistrCompositor - basic properties", {
  expect_pipeop(PipeOpDistrCompositor$new())
})

task = tgen("simsurv")$generate(10)

test_that("PipeOpDistrCompositor - overwrite = FALSE", {
  gr = mlr3pipelines::ppl("distrcompositor", lrn("surv.kaplan", id = "k2"), overwrite = FALSE)
  expect_silent(gr$train(task))
  expect_equal(
    gr$predict(task)[[1]]$distr,
    lrn("surv.kaplan", id = "k2")$train(task)$predict(task)$distr)
})

test_that("PipeOpDistrCompositor - overwrite = TRUE", {
  gr = mlr3pipelines::ppl("distrcompositor", lrn("surv.kaplan", id = "k2"), overwrite = TRUE, form = "ph")
  expect_silent(gr$train(task))
  p = gr$predict(task)[[1]]
  expect_prediction_surv(p)
  expect_true("distr" %in% p$predict_types)

  gr = mlr3pipelines::ppl("distrcompositor", lrn("surv.kaplan", id = "k2"), overwrite = TRUE, form = "po",
           graph_learner = TRUE)
  expect_silent(expect_prediction_surv(gr$train(task)$predict(task)))
})


test_that("no params", {
  base = lrn("surv.kaplan")$train(task)$predict(task)
  pred = lrn("surv.kaplan", id = "k2")$train(task)$predict(task)
  pod = mlr3pipelines::po("distrcompose", param_vals = list())
  expect_silent(pod$predict(list(base = base, pred = pred)))
})
