task = tgen("simsurv")$generate(20)

test_that("Construction", {
  p = PredictionSurv$new(row_ids = task$row_ids, truth = task$truth(), crank = runif(task$nrow))
  expect_prediction_surv(p)
})

test_that("Internally constructed Prediction", {
  lrn = lrn("surv.kaplan")
  p = lrn$train(task)$predict(task)
  expect_prediction_surv(p)
})

lrn = lrn("surv.kaplan")

test_that("c", {
  resampling = rsmp("cv", folds = 2)
  rr = resample(task, lrn, resampling)

  preds = rr$predictions()

  pred = do.call(c, preds)
  expect_prediction_surv(pred)

  dt = as.data.table(pred)
  expect_data_table(dt, nrows = task$nrow, ncols = 5L, any.missing = FALSE)

  p1 = lrn("surv.kaplan")$train(task)$predict(task)
  p2 = lrn("surv.coxph")$train(task)$predict(task)
  expect_error(c(p1, p2), "Cannot combine")

  resampling = rsmp("cv", folds = 2)
  rr = resample(task, lrn, resampling)

  preds = rr$predictions()

  pred = do.call(c, preds)
  expect_prediction_surv(pred)

  p1 = lrn("surv.kaplan")$train(task)$predict(task)
  p2 = p1
  expect_equal(length(c(p1, p2, keep_duplicates = TRUE)$row_ids), 40)
  expect_equal(length(c(p1, p2, keep_duplicates = FALSE)$row_ids), 20)
})
