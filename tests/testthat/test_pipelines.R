task = tsk("rats")$filter(sample(300, 50L))
task_regr = tgen("friedman1")$generate(20L)

test_that("crankcompositor", {
  pipe = mlr3pipelines::ppl("crankcompositor", learner = lrn("surv.kaplan"))
  expect_class(pipe, "Graph")
  pipe = mlr3pipelines::ppl("crankcompositor", learner = lrn("surv.kaplan"), graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p)
  expect_true("crank" %in% p$predict_types)
})

test_that("distrcompositor", {
  pipe = mlr3pipelines::ppl("distrcompositor", learner = lrn("surv.rpart"))
  expect_class(pipe, "Graph")

  pipe = mlr3pipelines::ppl("distrcompositor", learner = lrn("surv.rpart"), graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")

  p = pipe$train(task)$predict(task)
  expect_prediction_surv(p)
  expect_true("distr" %in% p$predict_types)
})

test_that("survaverager", {
  pipe = mlr3pipelines::ppl("survaverager", learners = list(lrn("surv.kaplan"),
    lrn("surv.kaplan", id = "k2")))
  expect_class(pipe, "Graph")

  pipe = mlr3pipelines::ppl("survaverager", learners = list(lrn("surv.kaplan"),
    lrn("surv.kaplan", id = "k2")),
  graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")

  expect_prediction_surv(pipe$train(task)$predict(task))
})

test_that("survbagging", {
  pipe = mlr3pipelines::ppl("survbagging", lrn("surv.kaplan"), iterations = 2)
  expect_class(pipe, "Graph")
  pipe = mlr3pipelines::ppl("survbagging", lrn("surv.kaplan"), iterations = 2, graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p)
})

test_that("resample survtoregr", {
  pipe = mlr3pipelines::ppl("survtoregr", method = 1, distrcompose = FALSE, graph_learner = TRUE)
  rr = resample(task, pipe, rsmp("cv", folds = 2L))
  expect_numeric(rr$aggregate())
})

test_that("survtoregr 1", {
  pipe = mlr3pipelines::ppl("survtoregr", method = 1, distrcompose = FALSE)
  expect_class(pipe, "Graph")
  pipe = mlr3pipelines::ppl("survtoregr", method = 1, distrcompose = FALSE, graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p)

  pipe = mlr3pipelines::ppl("survtoregr", method = 1, distrcompose = TRUE, graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p)
  expect_true("distr" %in% p$predict_types)
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

skip_if_not_installed("mlr3learners")

test_that("survtoclassif", {
  requireNamespace("mlr3learners")

  pipe = mlr3pipelines::ppl("survtoclassif", learner = lrn("classif.log_reg"))
  expect_class(pipe, "Graph")
  grlrn = mlr3pipelines::ppl("survtoclassif", learner = lrn("classif.log_reg"),
                             graph_learner = TRUE)
  expect_class(grlrn, "GraphLearner")
  grlrn$train(task)
  p = grlrn$predict(task)
  expect_prediction_surv(p)

  cox = lrn("surv.coxph")
  suppressWarnings(cox$train(task))
  p2 = cox$predict(task)

  expect_equal(p$truth, p2$truth)
  expect_equal(p$score(), p2$score(), tolerance = 0.1)

  # Test with cut
  grlrn = mlr3pipelines::ppl("survtoclassif", learner = lrn("classif.log_reg"),
                            cut = c(10, 30, 50), graph_learner = TRUE)
  expect_class(grlrn, "GraphLearner")
  suppressWarnings(grlrn$train(task))
  p = grlrn$predict(task)
  expect_prediction_surv(p)

  # Test with max event time
  max_time = task$data()[status == 1, min(time)]
  grlrn = mlr3pipelines::ppl("survtoclassif", learner = lrn("classif.log_reg"),
                             max_time = max_time, graph_learner = TRUE)
  expect_error(grlrn$train(task))

  grlrn = mlr3pipelines::ppl("survtoclassif", learner = lrn("classif.log_reg"),
                             max_time = max_time + 1, graph_learner = TRUE)
  suppressWarnings(grlrn$train(task))
  p = grlrn$predict(task)
  expect_prediction_surv(p)

  # Test with rhs
  grlrn = ppl("survtoclassif", learner = lrn("classif.log_reg"), rhs = "1",
              graph_learner = TRUE)
  grlrn$train(task)
  pred = grlrn$predict(task)

  grlrn = ppl("survtoclassif", learner = lrn("classif.featureless"), graph_learner = TRUE)
  grlrn$train(task)
  pred2 = grlrn$predict(task)

  expect_equal(pred$data$distr, pred2$data$distr)

  grlrn = ppl("survtoclassif", learner = lrn("classif.log_reg"), rhs = "rx + litter",
             graph_learner = TRUE)
  grlrn$train(task)
  pred = grlrn$predict(task)

  grlrn2 = ppl("survtoclassif", learner = lrn("classif.log_reg"), rhs = ".",
               graph_learner = TRUE)
  grlrn2$train(task)
  pred2 = suppressWarnings(grlrn2$predict(task))

  expect_lt(pred$score(), pred2$score())
})
