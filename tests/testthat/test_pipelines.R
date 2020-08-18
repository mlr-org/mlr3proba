task = tgen("simsurv")$generate(20)
task_regr = tgen("friedman1")$generate(20)

test_that("crankcompositor", {
  pipe = ppl("crankcompositor", learner = lrn("surv.kaplan"))
  expect_class(pipe, "Graph")
  pipe = ppl("crankcompositor", learner = lrn("surv.kaplan"), graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p)
  expect_true("crank" %in% p$predict_types)
})

test_that("distrcompositor", {
  pipe = ppl("distrcompositor", learner = lrn("surv.coxph"))
  expect_class(pipe, "Graph")
  pipe = ppl("distrcompositor", learner = lrn("surv.coxph"), graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p)
  expect_true("distr" %in% p$predict_types)
})

test_that("survaverager", {
  pipe = ppl("survaverager", learners = list(lrn("surv.coxph"), lrn("surv.kaplan"),
                                             lrn("surv.kaplan", id = "k2")))
  expect_class(pipe, "Graph")
  pipe = ppl("survaverager", learners = list(lrn("surv.coxph"), lrn("surv.kaplan"),
                                             lrn("surv.kaplan", id = "k2")),
             graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p)
})

test_that("survbagging", {
  pipe = ppl("survbagging", lrn("surv.kaplan"), iterations = 2)
  expect_class(pipe, "Graph")
  pipe = ppl("survbagging", lrn("surv.kaplan"), iterations = 2, graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p)
})

test_that("survtoregr 1", {
  pipe = ppl("survtoregr", method = 1, distrcompose = FALSE)
  expect_class(pipe, "Graph")
  pipe = ppl("survtoregr", method = 1, distrcompose = FALSE, graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p)

  pipe = ppl("survtoregr", method = 1, distrcompose = TRUE, graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p)
  expect_true("distr" %in% p$predict_types)
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
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p)

  pipe = ppl("survtoregr", method = 3, distrcompose = TRUE, graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p)
  expect_true("distr" %in% p$predict_types)
})
