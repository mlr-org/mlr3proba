test_that("basic properties", {
  expect_pipeop(PipeOpDistrCompositor$new())

  # check that during construction, initial values are not overwritten
  values = PipeOpDistrCompositor$new()$param_set$values
  values2 = PipeOpDistrCompositor$new(param_vals = list(overwrite = FALSE))$param_set$values
  expect_equal(values, values2)
})

set.seed(42L)
task = tsk("rats")$filter(sample(300, 110L))
cox_pred = lrn("surv.coxph")$train(task)$predict(task)

test_that("no params", {
  base = lrn("surv.kaplan")$train(task)$predict(task)
  pred = lrn("surv.kaplan")$train(task)$predict(task)
  pod = mlr3pipelines::po("distrcompose", param_vals = list())
  expect_silent(pod$predict(list(base = base, pred = pred)))
})

test_that("overwrite = FALSE", {
  gr = mlr3pipelines::ppl("distrcompositor", lrn("surv.kaplan"), overwrite = FALSE)
  expect_class(gr, "Graph")
  expect_silent(gr$train(task))
  expect_identical(
    gr$predict(task)[[1L]]$data$distr,
    lrn("surv.kaplan")$train(task)$predict(task)$data$distr
  )

  # breslow
  gr = mlr3pipelines::ppl("distrcompositor", lrn("surv.coxph"),
    estimator = "breslow", overwrite = FALSE)
  expect_silent(gr$train(task))
  expect_identical(gr$predict(task)[[1L]]$data$distr, cox_pred$data$distr)
})

test_that("overwrite = TRUE", {
  gr = mlr3pipelines::ppl("distrcompositor", lrn("surv.kaplan"), overwrite = TRUE, form = "ph")
  expect_class(gr, "Graph")
  expect_silent(gr$train(task))
  p = gr$predict(task)[[1L]]
  expect_prediction_surv(p)
  expect_true("distr" %in% p$predict_types)

  grlrn = mlr3pipelines::ppl("distrcompositor", learner = lrn("surv.coxph"),
                             overwrite = TRUE, form = "po", graph_learner = TRUE)
  expect_class(grlrn, "GraphLearner")
  p = grlrn$train(task)$predict(task)
  expect_prediction_surv(p)
  expect_false(all(p$data$distr == cox_pred$data$distr)) # PO distr != Cox's Breslow default

  grlrn = mlr3pipelines::ppl("distrcompositor", learner = lrn("surv.coxph"),
                             overwrite = TRUE, form = "aft", graph_learner = TRUE)
  p = grlrn$train(task)$predict(task)
  expect_false(all(p$data$distr == cox_pred$data$distr)) # AFT distr != Cox's Breslow default

  # our breslow seems different from Cox's breslow
  gr = mlr3pipelines::ppl("distrcompositor", learner = lrn("surv.coxph"),
                          estimator = "breslow", overwrite = TRUE, graph_learner = TRUE)
  p = grlrn$train(task)$predict(task)
  expect_false(all(p$data$distr == cox_pred$data$distr)) # distr predictions changed (a bit)
})

test_that("composition from crank doesn't work", {
  grlrn = mlr3pipelines::ppl("distrcompositor", learner = lrn("surv.rpart"), graph_learner = TRUE)
  p = grlrn$train(task)$predict(task)
  # rpart has only crank prediction, so no distr composition can be made
  expect_false("distr" %in% p$predict_types)
})
