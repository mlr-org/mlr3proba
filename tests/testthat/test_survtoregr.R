skip("Due to bugs in survtoregr methods")
test_that("resample survtoregr", {
  grlrn = ppl("survtoregr", method = 1, distrcompose = FALSE, graph_learner = TRUE)
  rr = resample(task, grlrn, rsmp("cv", folds = 2L))
  expect_numeric(rr$aggregate())
})

test_that("survtoregr 1", {
  pipe = ppl("survtoregr", method = 1)
  expect_class(pipe, "Graph")
  grlrn = ppl("survtoregr", method = 1, graph_learner = TRUE)
  expect_class(grlrn, "GraphLearner")
  p = grlrn$train(task)$predict(task)
  expect_prediction_surv(p)
  expect_true("response" %in% p$predict_types)
})

test_that("survtoregr 2", {
  pipe = ppl("survtoregr", method = 2)
  expect_class(pipe, "Graph")
  pipe = ppl("survtoregr", method = 2, graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p)
  expect_true("distr" %in% p$predict_types)

  pipe = ppl("survtoregr", method = 2, regr_se_learner = lrn("regr.featureless"),
             graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p)
  expect_true("distr" %in% p$predict_types)
})

test_that("survtoregr 3", {
  pipe = ppl("survtoregr", method = 3, distrcompose = FALSE)
  expect_class(pipe, "Graph")
  pipe = ppl("survtoregr", method = 3, distrcompose = FALSE, graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  suppressWarnings(pipe$train(task)) # suppress loglik warning
  p = pipe$predict(task)
  expect_prediction_surv(p)

  pipe = ppl("survtoregr", method = 3, distrcompose = TRUE, graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  suppressWarnings(pipe$train(task)) # suppress loglik warning
  p = pipe$predict(task)
  expect_prediction_surv(p)
  expect_true("distr" %in% p$predict_types)
})
