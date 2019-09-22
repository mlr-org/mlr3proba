context("PredictionSurv")

test_that("Construction", {
  task = tsk("lung")
  p = PredictionSurv$new(row_ids = task$row_ids, truth = task$truth(), risk = runif(task$nrow))
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
  expect_data_table(dt, nrows = task$nrow, ncols = 4L, any.missing = FALSE)
})
