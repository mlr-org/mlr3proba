context("PredictionSurv")

test_that("Construction", {
  task = tsk("lung")
  p = PredictionSurv$new(row_ids = task$row_ids, truth = task$truth(), crank = runif(task$nrow))
  expect_prediction_surv(p)
})

test_that("Internally constructed Prediction", {
  task = tsk("lung")
  lrn = lrn("surv.rpart")
  p = lrn$train(task)$predict(task)
  expect_prediction_surv(p)
})

test_that("c", {
  task = tsk("lung")
  lrn = lrn("surv.rpart")
  resampling = rsmp("cv", folds = 3)
  rr = resample(task, lrn, resampling)

  preds = rr$predictions()

  pred = do.call(c, preds)
  expect_prediction_surv(pred)

  dt = as.data.table(pred)
  expect_data_table(dt, nrows = task$nrow, ncols = 5L, any.missing = FALSE)

  task = tsk("rats")
  p1 = lrn("surv.kaplan")$train(task)$predict(task)
  p2 = lrn("surv.glmnet")$train(task)$predict(task)
  expect_error(c(p1,p2), "Cannot rbind")

  lrn = lrn("surv.glmnet")
  resampling = rsmp("cv", folds = 3)
  rr = resample(task, lrn, resampling)

  preds = rr$predictions()

  pred = do.call(c, preds)
  expect_prediction_surv(pred)

  p1 = lrn("surv.glmnet")$train(task)$predict(task)
  p2 = p1
  expect_equal(length(c(p1, p2, keep_duplicates = TRUE)$row_ids), 600)
  expect_equal(length(c(p1, p2, keep_duplicates = FALSE)$row_ids), 300)
})

