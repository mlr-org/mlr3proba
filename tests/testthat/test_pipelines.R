task = tsk("rats")$filter(sample(300, 50L))
task_regr = tgen("friedman1")$generate(20L)

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
  expect_equal(p$score(), p2$score(), tolerance = 0.015)

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
  task = tsk("lung")
  part = partition(task)
  task_train = task$clone()$filter(part$train)
  task_test = task$clone()$filter(part$test)

  pipe = mlr3pipelines::ppl("survtoclassif_IPCW", learner = lrn("classif.rpart"),
                            cutoff_time = 500)
  expect_class(pipe, "Graph")

  grlrn = mlr3pipelines::ppl("survtoclassif_IPCW", learner = lrn("classif.rpart"),
                             cutoff_time = 500, graph_learner = TRUE)
  expect_class(grlrn, "GraphLearner")
  grlrn$train(task_train)
  # check: weights were used
  expect_vector(grlrn$model$classif.rpart$model$call$weights, ptype = numeric(),
                size = task_train$nrow)
  p = grlrn$predict(task_test)
  expect_prediction_surv(p)
  # check crank and distr exist
  expect_true("crank" %in% names(p$data))
  # p$data$distr => 1 column, cutoff time as columname
  expect_matrix(p$data$dist, nrows = nrow(task_test$nrow), ncols = 1)
  expect_true(colnames(p$data$dist) == "500")

  # Test with different cutoff_time (fix cutoff code before during predict phase)
  grlrn = mlr3pipelines::ppl("survtoclassif_IPCW", learner = lrn("classif.rpart"),
                             cutoff_time = 600, graph_learner = TRUE)
  grlrn$train(task_train)
  p2 = grlrn$predict(task_test)

  # different cutoff time, different crank predictions
  expect_false(all(p$crank == p2$crank))

  # check msr("surv.brier") with only one time point? Eg prob at cutoff time?
})
