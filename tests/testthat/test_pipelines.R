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

  pipe = mlr3pipelines::ppl("survtoclassif", mlr3learners::LearnerClassifLogReg$new())
  expect_class(pipe, "Graph")
  pipe = mlr3pipelines::ppl("survtoclassif", mlr3learners::LearnerClassifLogReg$new(), graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  pipe$train(task)
  p = pipe$predict(task)
  expect_prediction_surv(p)

  cox = lrn("surv.coxph")
  cox$train(task) |> suppressWarnings()
  p2 = cox$predict(task)

  expect_equal(p$truth, p2$truth)
  expect_equal(p$score(), p2$score(), tolerance = 0.1)

  # Test with cut
  pipe = mlr3pipelines::ppl("survtoclassif", mlr3learners::LearnerClassifLogReg$new(), cut = c(10, 30, 50), graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  pipe$train(task) |> suppressWarnings()
  p = pipe$predict(task)
  expect_prediction_surv(p)

  # Test with max_time
  t = task$data()[status == 1, min(time)]
  pipe = mlr3pipelines::ppl("survtoclassif", mlr3learners::LearnerClassifLogReg$new(), max_time = t, graph_learner = TRUE)
  expect_class(pipe, "GraphLearner")
  expect_error(pipe$train(task))

  pipe = mlr3pipelines::ppl("survtoclassif", mlr3learners::LearnerClassifLogReg$new(), max_time = t + 1, graph_learner = TRUE)
  pipe$train(task) |> suppressWarnings()
  p = pipe$predict(task)
  expect_prediction_surv(p)

  # Test with rhs
  pipe = ppl("survtoclassif", learner = lrn("classif.log_reg"), rhs = "1", graph_learner = TRUE)
  pipe$train(task)
  pred = pipe$predict(task)

  pipe = ppl("survtoclassif", learner = lrn("classif.featureless"), graph_learner = TRUE)
  pipe$train(task)
  pred2 = pipe$predict(task)

  expect_equal(pred$data$distr, pred2$data$distr)

  pipe = ppl("survtoclassif", learner = lrn("classif.log_reg"), rhs = "rx + litter", graph_learner = TRUE)
  pipe$train(task)
  pred = pipe$predict(task)

  pipe = ppl("survtoclassif", learner = lrn("classif.log_reg"), rhs = ".", graph_learner = TRUE)
  pipe$train(task)
  pred2 = pipe$predict(task) |> suppressWarnings()

  expect_true(pred$score() < pred2$score())
})
