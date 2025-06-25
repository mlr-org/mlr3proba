test_that("basic properties", {
  expect_pipeop(PipeOpCrankCompositor$new())
  expect_equal(PipeOpCrankCompositor$new()$param_set$values$method, "mort")

  # check that during construction, initial values are not overwritten
  values = PipeOpCrankCompositor$new()$param_set$values
  values2 = PipeOpCrankCompositor$new(param_vals = list(method = "mort"))$param_set$values
  expect_equal(values, values2)
})

set.seed(42)
task = tgen("simsurv")$generate(20L)
pcox = lrn("surv.coxph")$train(task)$predict(task)

test_that("no params", {
  po = PipeOpCrankCompositor$new(param_vals = list())
  pred_km = lrn("surv.kaplan")$train(task)$predict(task)
  po$train(list(NULL))
  p = po$predict(list(pred_km))[[1L]]
  expect_prediction_surv(p)
  # by default, no overwrite happens
  expect_identical(p$crank, pred_km$crank)
})

test_that("overwrite crank", {
  # no overwrite
  poc = po("crankcompose")
  poc$train(list(NULL))
  p1 = poc$predict(list(pcox))[[1L]]
  expect_identical(p1$crank, pcox$crank)

  # overwrite crank
  poc = po("crankcompose", param_vals = list(overwrite = TRUE))
  poc$train(list(NULL))
  p2 = poc$predict(list(pcox))[[1L]]
  expect_false(all(p2$crank == pcox$crank))

  # even if prediction doesn't have crank somehow, pipeop will add it even if no overwrite
  p2$data$crank = NULL
  expect_true(all(is.na(p2$crank))) # NAs produce so that c-index can be calculated

  poc = po("crankcompose", param_vals = list(overwrite = FALSE))
  poc$train(list(NULL))
  p3 = poc$predict(list(p2))[[1L]]
  expect_false(any(is.na(p3$crank)))
})

test_that("pipeline works", {
  pipe = ppl("crankcompositor", learner = lrn("surv.kaplan"))
  expect_class(pipe, "Graph")

  grlrn = ppl("crankcompositor", learner = lrn("surv.kaplan"), graph_learner = TRUE)
  expect_class(grlrn, "GraphLearner")
  p = grlrn$train(task)$predict(task)
  expect_prediction_surv(p)
  expect_true("crank" %in% p$predict_types)
})
