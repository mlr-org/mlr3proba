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

test_that("survtoclassif_disctime", {
  requireNamespace("mlr3learners")

  pipe = mlr3pipelines::ppl("survtoclassif_disctime", learner = lrn("classif.log_reg"))
  expect_class(pipe, "Graph")
  grlrn = mlr3pipelines::ppl("survtoclassif_disctime", learner = lrn("classif.log_reg"),
                             graph_learner = TRUE)
  expect_class(grlrn, "GraphLearner")
  grlrn$train(task)
  p = grlrn$predict(task)
  expect_prediction_surv(p)

  # compare with simple cox prediction
  cox = lrn("surv.coxph")
  suppressWarnings(cox$train(task))
  p2 = cox$predict(task)

  expect_equal(p$row_ids, p2$row_ids)
  expect_equal(p$truth, p2$truth)
  expect_equal(p$score(), p2$score(), tolerance = 0.01)

  # Test with cut
  grlrn = mlr3pipelines::ppl("survtoclassif_disctime", learner = lrn("classif.log_reg"),
                            cut = c(10, 30, 50), graph_learner = TRUE)
  expect_class(grlrn, "GraphLearner")
  suppressWarnings(grlrn$train(task))
  p = grlrn$predict(task)
  expect_prediction_surv(p)

  # `max_time` needs to be larger than the minimum event time so we choose
  # the minimum event time in the data for testing
  max_time = task$data()[status == 1, min(time)]
  grlrn = mlr3pipelines::ppl("survtoclassif_disctime", learner = lrn("classif.log_reg"),
                             max_time = max_time, graph_learner = TRUE)
  expect_error(grlrn$train(task))

  grlrn = mlr3pipelines::ppl("survtoclassif_disctime", learner = lrn("classif.log_reg"),
                             max_time = max_time + 1, graph_learner = TRUE)
  suppressWarnings(grlrn$train(task))
  p = grlrn$predict(task)
  expect_prediction_surv(p)

  # Test with rhs
  grlrn = mlr3pipelines::ppl("survtoclassif_disctime", learner = lrn("classif.log_reg"),
                             rhs = "1", graph_learner = TRUE)
  grlrn$train(task)
  pred = suppressWarnings(grlrn$predict(task))

  grlrn2 = mlr3pipelines::ppl("survtoclassif_disctime", learner = lrn("classif.featureless"),
                              graph_learner = TRUE)
  grlrn2$train(task)
  pred2 = grlrn2$predict(task)

  # featureless has random discrimination
  expect_equal(unname(pred$score()), 0.5)
  expect_equal(unname(pred2$score()), 0.5)
  expect_equal(pred$data$distr, pred2$data$distr)

  grlrn = mlr3pipelines::ppl("survtoclassif_disctime", learner = lrn("classif.log_reg"),
                             rhs = "rx + litter", graph_learner = TRUE)
  grlrn$train(task)
  pred = suppressWarnings(grlrn$predict(task))

  grlrn2 = mlr3pipelines::ppl("survtoclassif_disctime", learner = lrn("classif.log_reg"),
                              rhs = ".", graph_learner = TRUE)
  grlrn2$train(task)
  pred2 = suppressWarnings(grlrn2$predict(task))

  # model with more covariates should have better C-index
  expect_gt(pred2$score(), pred$score())
})

skip_if_not_installed("mlr3extralearners")

test_that("survtoclassif_IPCW", {
  requireNamespace("mlr3extralearners")

  pipe = mlr3pipelines::ppl("survtoclassif_IPCW", learner = lrn("classif.gam"))
  expect_class(pipe, "Graph")

  ## This needs fixing
  grlrn = mlr3pipelines::ppl("survtoclassif_IPCW", learner = lrn("classif.gam"),
                             graph_learner = TRUE)
  expect_class(grlrn, "GraphLearner")
  grlrn$train(task)
  p = grlrn$predict(task)
  expect_prediction_surv(p)

  # Test with cutoff_time
  grlrn = mlr3pipelines::ppl("survtoclassif_IPCW", learner = lrn("classif.gam"),
                             cutoff_time = 50)
  expect_class(pipe, "Graph")
  suppressWarnings(grlrn$train(task))
  p1 = grlrn$predict(task)
  expect_prediction_classif(p1$classif.gam.output)

  grlrn = mlr3pipelines::ppl("survtoclassif_IPCW", learner = lrn("classif.gam"),
                             cutoff_time = 75)
  suppressWarnings(grlrn$train(task))
  p2 = grlrn$predict(task)

  expect_false(any(p1$classif.gam.output$data$prob == p2$classif.gam.output$data$prob))
})
