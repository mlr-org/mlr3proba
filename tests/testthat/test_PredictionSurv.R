set.seed(1)
task = tsk("rats")$filter(sample(300, 20))

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
  p2 = suppressWarnings(lrn("surv.coxph")$train(task))$predict(task)
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

test_that("data.frame roundtrip", {
  p1 = suppressWarnings(lrn("surv.coxph")$train(task)$predict(task))

  tab = as.data.table(p1)
  p2 = as_prediction_surv(tab)
  expect_prediction_surv(p2)

  expect_equal(
    suppressWarnings(as.data.table(p1$distr$parameters())$value),
    suppressWarnings(as.data.table(p2$distr$parameters())$value)
  )

  expect_equal(as.data.table(p1)[, -6L], as.data.table(p2)[, -6L])
})

test_that("as_prediction_surv", {
  p = suppressWarnings(lrn("surv.coxph")$train(task))$predict(task)
  expect_prediction_surv(as_prediction_surv(as.data.table(p)))
})

test_that("filtering", {
  p = suppressWarnings(lrn("surv.coxph")$train(task)$predict(task))
  p$filter(c(20, 37, 42))
  expect_prediction_surv(p)

  expect_set_equal(p$data$row_ids, c(20, 37, 42))
  expect_numeric(p$data$crank, any.missing = FALSE, len = 3)
  expect_numeric(p$data$lp, any.missing = FALSE, len = 3)
  expect_matrix(p$data$distr, nrows = 3)
})
