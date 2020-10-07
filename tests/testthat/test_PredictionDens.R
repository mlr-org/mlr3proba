task = tsk("precip")
lrn = lrn("dens.hist")

test_that("Construction", {
  p = PredictionDens$new(task)
  expect_prediction_dens(p)
})

test_that("Internally constructed Prediction", {
  p = lrn$train(task)$predict(task)
  expect_prediction_dens(p)
})

test_that("c", {
  resampling = rsmp("cv", folds = 3)
  rr = resample(task, lrn, resampling)

  preds = rr$predictions()

  pred = do.call(c, preds)
  expect_prediction_dens(pred)

  dt = as.data.table(pred)
  expect_data_table(dt, nrows = task$nrow, ncols = 3L, any.missing = FALSE)
})
