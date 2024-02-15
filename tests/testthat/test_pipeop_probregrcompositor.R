test_that("PipeOpProbregr - basic properties", {
  expect_pipeop(PipeOpProbregr$new())
  expect_pipeop(PipeOpProbregr$new(param_vals = list()))

  # check that during construction, initial values are not overwritten
  values  = PipeOpProbregr$new()$param_set$values
  values2 = PipeOpProbregr$new(param_vals = list(dist = "Uniform"))$param_set$values
  expect_equal(values, values2)
})

task = tgen("friedman1")$generate(10)

test_that("PipeOpProbregr", {
  gr = mlr3pipelines::ppl("probregr", lrn("regr.featureless", predict_type = "se"),
    graph_learner = TRUE)
  p = gr$train(task)$predict(task)
  expect_true(distr6::testDistribution(p$distr))
  expect_equal(unique(p$distr$modelTable$Distribution), "Uniform")
})

test_that("no se", {
  expect_error(mlr3pipelines::ppl("probregr", lrn("regr.rpart"),
    graph_learner = TRUE), "does not support")
})

test_that("probregr_compose", {
  gr = mlr3pipelines::ppl("probregr",
    learner = lrn("regr.featureless", predict_type = "se"),
    dist = "Logistic", graph_learner = TRUE)$train(task)$predict(task)
  expect_true(distr6::testDistribution(gr$distr))
})
