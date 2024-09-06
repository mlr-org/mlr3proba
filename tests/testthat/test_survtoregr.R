skip("Due to bugs in survtoregr methods")
test_that("resample survtoregr", {
  grlrn = mlr3pipelines::ppl("survtoregr", method = 1, distrcompose = FALSE, graph_learner = TRUE)
  rr = resample(task, grlrn, rsmp("cv", folds = 2L))
  expect_numeric(rr$aggregate())
})

test_that("survtoregr 1", {
  pipe = mlr3pipelines::ppl("survtoregr", method = 1)
  expect_class(pipe, "Graph")
  grlrn = mlr3pipelines::ppl("survtoregr", method = 1, graph_learner = TRUE)
  expect_class(grlrn, "GraphLearner")
  p = grlrn$train(task)$predict(task)
  expect_prediction_surv(p)
  expect_true("response" %in% p$predict_types)
})

test_that("survtoregr 2", {
  pipe = mlr3pipelines::ppl("survtoregr", method = 2)
  expect_class(pipe, "Graph")
  pipe = mlr3pipelines::ppl("survtoregr", method = 2, graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p)
  expect_true("distr" %in% p$predict_types)

  pipe = mlr3pipelines::ppl("survtoregr", method = 2, regr_se_learner = lrn("regr.featureless"),
                            graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p)
  expect_true("distr" %in% p$predict_types)
})

test_that("survtoregr 3", {
  pipe = mlr3pipelines::ppl("survtoregr", method = 3, distrcompose = FALSE)
  expect_class(pipe, "Graph")
  pipe = mlr3pipelines::ppl("survtoregr", method = 3, distrcompose = FALSE,
                            graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  suppressWarnings(pipe$train(task)) # suppress loglik warning
  p = pipe$predict(task)
  expect_prediction_surv(p)

  pipe = mlr3pipelines::ppl("survtoregr", method = 3, distrcompose = TRUE,
                            graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  suppressWarnings(pipe$train(task)) # suppress loglik warning
  p = pipe$predict(task)
  expect_prediction_surv(p)
  expect_true("distr" %in% p$predict_types)
})
