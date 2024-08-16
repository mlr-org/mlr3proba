test_that("basic properties", {
  expect_pipeop(PipeOpCrankCompositor$new())
  expect_equal(PipeOpCrankCompositor$new()$param_set$values$method, "mort")

  # check that during construction, initial values are not overwritten
  values = PipeOpCrankCompositor$new()$param_set$values
  values2 = PipeOpCrankCompositor$new(param_vals = list(method = "mort"))$param_set$values
  expect_equal(values, values2)
})

set.seed(2218L)
task = tgen("simsurv")$generate(20L)

test_that("no params", {
  po = PipeOpCrankCompositor$new(param_vals = list())
  pred_km = lrn("surv.kaplan")$train(task)$predict(task)
  p = po$predict(list(pred_km))[[1L]]
  expect_prediction_surv(p)
  # by default, no overwrite happens
  expect_identical(p$crank, pred_km$crank)
})

test_that("pipeline works", {
  pipe = mlr3pipelines::ppl("crankcompositor", learner = lrn("surv.kaplan"))
  expect_class(pipe, "Graph")

  grlrn = mlr3pipelines::ppl("crankcompositor", learner = lrn("surv.kaplan"), graph_learner = TRUE)
  expect_class(grlrn, "GraphLearner")
  p = grlrn$train(task)$predict(task)
  expect_prediction_surv(p)
  expect_true("crank" %in% p$predict_types)
})

test_that("overwrite crank", {
  # no overwrite
  grlrn = mlr3pipelines::ppl("crankcompositor", lrn("surv.coxph"), graph_learner = TRUE)
  p1 = grlrn$train(task)$predict(task)
  p2 = lrn("surv.coxph")$train(task)$predict(task)
  expect_identical(p1$crank, p2$crank)

  # overwrite crank
  grlrn = mlr3pipelines::ppl("crankcompositor", lrn("surv.coxph"), graph_learner = TRUE,
                             overwrite = TRUE)
  p1 = grlrn$train(task)$predict(task)
  p2 = lrn("surv.coxph")$train(task)$predict(task)
  expect_false(all(p1$crank == p2$crank))
})
