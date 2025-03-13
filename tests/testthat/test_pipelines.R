task = tsk("rats")$filter(sample(300, 50L))
task_regr = tgen("friedman1")$generate(20L)

test_that("survaverager", {
  learners = list(lrn("surv.kaplan"), lrn("surv.kaplan", id = "k2"))
  pipe = ppl("survaverager", learners = learners)
  expect_class(pipe, "Graph")

  grlrn = ppl("survaverager", learners = learners, graph_learner = TRUE)
  expect_class(grlrn, "GraphLearner")

  expect_prediction_surv(grlrn$train(task)$predict(task))
})

test_that("survbagging", {
  pipe = ppl("survbagging", lrn("surv.kaplan"), iterations = 2)
  expect_class(pipe, "Graph")
  grlrn = ppl("survbagging", lrn("surv.kaplan"), iterations = 2, graph_learner = TRUE)
  expect_class(grlrn, "GraphLearner")
  p = grlrn$train(task)$predict(task)
  expect_prediction_surv(p)
})

test_that("survtoclassif_disctime", {
  skip_if_not_installed("mlr3learners")

  pipe = ppl("survtoclassif_disctime", learner = lrn("classif.log_reg"))
  expect_class(pipe, "Graph")
  grlrn = ppl("survtoclassif_disctime", learner = lrn("classif.log_reg"),
              graph_learner = TRUE)
  expect_class(grlrn, "GraphLearner")
  expect_equal(grlrn$predict_type, "crank")
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
  grlrn = ppl("survtoclassif_disctime", learner = lrn("classif.log_reg"),
              cut = c(10, 30, 50), graph_learner = TRUE)
  expect_class(grlrn, "GraphLearner")
  suppressWarnings(grlrn$train(task))
  p = grlrn$predict(task)
  expect_prediction_surv(p)

  # `max_time` needs to be larger than the minimum event time so we choose
  # the minimum event time in the data for testing
  max_time = task$data()[status == 1, min(time)]
  grlrn = ppl("survtoclassif_disctime", learner = lrn("classif.log_reg"),
              max_time = max_time, graph_learner = TRUE)
  expect_error(grlrn$train(task))

  grlrn = ppl("survtoclassif_disctime", learner = lrn("classif.log_reg"),
              max_time = max_time + 1, graph_learner = TRUE)
  suppressWarnings(grlrn$train(task))
  p = grlrn$predict(task)
  expect_prediction_surv(p)

  # Test with rhs
  grlrn = ppl("survtoclassif_disctime", learner = lrn("classif.log_reg"),
              rhs = "1", graph_learner = TRUE)
  grlrn$train(task)
  pred = suppressWarnings(grlrn$predict(task))

  grlrn2 = ppl("survtoclassif_disctime", learner = lrn("classif.featureless"),
               graph_learner = TRUE)
  grlrn2$train(task)
  pred2 = grlrn2$predict(task)

  # featureless has random discrimination
  expect_equal(unname(pred$score()), 0.5)
  expect_equal(unname(pred2$score()), 0.5)
  expect_equal(pred$data$distr, pred2$data$distr)

  grlrn = ppl("survtoclassif_disctime", learner = lrn("classif.log_reg"),
              rhs = "rx + litter", graph_learner = TRUE)
  grlrn$train(task)
  pred = suppressWarnings(grlrn$predict(task))

  grlrn2 = ppl("survtoclassif_disctime", learner = lrn("classif.log_reg"),
               rhs = ".", graph_learner = TRUE)
  grlrn2$train(task)
  pred2 = suppressWarnings(grlrn2$predict(task))

  # model with more covariates should have better C-index
  expect_gt(pred2$score(), pred$score())
})

test_that("survtoclassif_IPCW", {
  task = tsk("lung")
  part = partition(task)

  pipe = ppl("survtoclassif_IPCW", learner = lrn("classif.rpart"), tau = 500)
  expect_class(pipe, "Graph")

  grlrn = ppl("survtoclassif_IPCW", learner = lrn("classif.rpart"), tau = 500,
              graph_learner = TRUE)
  expect_class(grlrn, "GraphLearner")
  expect_equal(grlrn$predict_type, "crank")

  grlrn$train(task, row_ids = part$train)
  # check that the weights were used for classif learner
  expect_numeric(grlrn$model$classif.rpart$model$call$weights, any.missing = FALSE,
                 len = length(part$train))
  p = grlrn$predict(task, row_ids = part$test)
  expect_prediction_surv(p)
  # crank is like risk => prob of having the event up to cutoff time
  expect_numeric(p$crank, len = length(part$test), lower = 0, upper = 1)
  # p$data$distr => 1 column, cutoff time as column name
  expect_matrix(p$data$distr, nrows = length(part$test), ncols = 1)
  expect_true(colnames(p$data$distr) == "500")
  # crank = risk = 1 - surv at cutoff time
  expect_equal(p$crank, 1 - p$data$distr[,"500"])
  # brier score at the cutoff time works
  expect_number(p$score(msr("surv.brier", times = 500, integrated = FALSE)), finite = TRUE)

  # Test with different tau
  grlrn = ppl("survtoclassif_IPCW", learner = lrn("classif.rpart"), tau = 600,
              graph_learner = TRUE)
  grlrn$train(task, part$train)
  p2 = grlrn$predict(task, part$test)

  # check predictions
  expect_numeric(p2$crank, len = length(part$test), lower = 0, upper = 1)
  expect_number(p2$score(msr("surv.brier", times = 600, integrated = FALSE)), finite = TRUE)

  # different cutoff times, different (crank) predictions
  expect_false(all(p$crank == p2$crank))
})

test_that("survtoregr_PEM", {
  skip_if_not_installed("mlr3learners")
  if (require_namespaces('glmnet', quietly = TRUE)){
      
    
    task = tsk('rats')
    # for this section, select only numeric covariates, 
    # as 'regr.glmnet' does not automatically handle factor type variables
    task$select(c('litter', 'rx'))
    learner = lrn('regr.glmnet', 
                  family = "poisson", 
                  lambda = 0,
                  use_pred_offset = FALSE)
    
  
    pipe = ppl("survtoregr_PEM", learner = learner)
    expect_class(pipe, "Graph")
    grlrn = ppl("survtoregr_PEM", learner = learner, graph_learner = TRUE)
    expect_class(grlrn, "GraphLearner")
    expect_equal(grlrn$predict_type, "response")
    
    grlrn$train(task)
    p = grlrn$predict(task)
    expect_prediction_surv(p)
    
    
    cox = lrn("surv.coxph")
    suppressWarnings(cox$train(task))
    p2 = cox$predict(task)
    
    expect_equal(p$row_ids, p2$row_ids)
    expect_equal(p$truth, p2$truth)
    expect_equal(p$score(), p2$score(), tolerance = 0.015) 
    
    # Test with cut
    grlrn = ppl("survtoregr_PEM", 
                learner = learner,
                cut = c(10, 30, 50),
                graph_learner = TRUE)
    expect_class(grlrn, "GraphLearner")
    suppressWarnings(grlrn$train(task))
    p = grlrn$predict(task)
    expect_prediction_surv(p)
    
    # `max_time` needs to be larger than the minimum event time so we choose
    # the minimum event time in the data for testing
    max_time = task$data()[status == 1, min(time)]
    
    grlrn = ppl("survtoregr_PEM", 
                learner = learner,
                max_time = max_time,
                graph_learner = TRUE)
    
    expect_error(grlrn$train(task))
    
    grlrn = ppl("survtoregr_PEM", 
                learner = learner,
                max_time = max_time + 1,
                graph_learner = TRUE)
    suppressWarnings(grlrn$train(task))
    p = grlrn$predict(task)
    expect_prediction_surv(p)
    
    # Test with rhs
    
    grlrn = ppl("survtoregr_PEM", 
                learner = learner,
                rhs = 'tend',
                graph_learner = TRUE)
    grlrn$train(task)
    pred = suppressWarnings(grlrn$predict(task))
    
    # tend as only covariate leads to random discrimination
    expect_equal(unname(pred$score()), 0.5)
    
    task = tsk('rats')
    grlrn = ppl("survtoregr_PEM", learner = learner,
                rhs = "rx + litter", graph_learner = TRUE)
    grlrn$train(task)
    pred = suppressWarnings(grlrn$predict(task))
    
    grlrn2 = ppl("survtoregr_PEM", learner = learner,
                 rhs = ".", graph_learner = TRUE)
    grlrn2$train(task)
    pred2 = suppressWarnings(grlrn2$predict(task))
    
    # model with more covariates should have better C-index
    expect_gt(pred2$score(), pred$score())
  }
})
  
