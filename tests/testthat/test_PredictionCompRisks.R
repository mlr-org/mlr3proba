set.seed(42L)
task = gen_cmprsk_task(n = 50, n_events = 3)
cif_list = gen_cif(n = 50, n_events = 3)
learner = lrn("cmprsk.aalen")

test_that("Construction", {
  p = PredictionCompRisks$new(
    row_ids = task$row_ids,
    truth = task$truth(),
    cif = cif_list
  )
  expect_prediction_cmprsk(p)
})

test_that("Internally constructed Prediction", {
  p = learner$train(task)$predict(task)
  expect_prediction_cmprsk(p)
})

test_that("combining predictions", {
  set.seed(1L)
  rr = resample(tsk("pbc"), lrn("cmprsk.aalen"), rsmp("cv", folds = 3L))
  preds = rr$predictions()

  # different time points in each resampling `preds[[i]]`
  # Note: AJ has the same number of time points independent of the cause, so we
  # just check for cause `1`
  expect_false(ncol(preds[[1L]]$data$cif$`1`) == ncol(preds[[2L]]$data$cif$`1`))
  expect_false(ncol(preds[[2L]]$data$cif$`1`) == ncol(preds[[3L]]$data$cif$`1`))

  # combine predictions
  pred = do.call(c, preds)
  expect_prediction_cmprsk(pred)

  # check that time points are properly combined
  # cause 1
  times1 = as.numeric(colnames(pred$cif$`1`))
  times1_fold1 = as.numeric(colnames(preds[[1L]]$data$cif$`1`))
  times1_fold2 = as.numeric(colnames(preds[[2L]]$data$cif$`1`))
  times1_fold3 = as.numeric(colnames(preds[[3L]]$data$cif$`1`))
  expect_true(all(times1 == sort(unique(c(times1_fold1, times1_fold2, times1_fold3)))))

  # cause 2
  times2 = as.numeric(colnames(pred$cif$`2`))
  times2_fold1 = as.numeric(colnames(preds[[1L]]$data$cif$`2`))
  times2_fold2 = as.numeric(colnames(preds[[2L]]$data$cif$`2`))
  times2_fold3 = as.numeric(colnames(preds[[3L]]$data$cif$`2`))
  expect_true(all(times2 == sort(unique(c(times2_fold1, times2_fold2, times2_fold3)))))

  # data.table conversion
  dt = as.data.table(pred)
  expect_data_table(dt, nrows = tsk("pbc")$nrow, ncols = 4L, any.missing = FALSE)
  # row ids are correctly combined
  expect_equal(dt$row_ids, c(preds[[1]]$row_ids, preds[[2]]$row_ids, preds[[3]]$row_ids))
})

test_that("data.frame roundtrip", {
  p1 = learner$train(task)$predict(task)
  tab = as.data.table(p1)
  p2 = as_prediction_cmprsk(tab)
  expect_prediction_cmprsk(p2)

  expect_equal(p1$cif, p2$cif)
  expect_equal(tab[, !("CIF")], as.data.table(p2)[, !("CIF")])
})

test_that("as_prediction_cmprsk", {
  p = lrn("cmprsk.aalen")$train(task)$predict(task)
  expect_prediction_cmprsk(as_prediction_cmprsk(as.data.table(p)))
})

test_that("filtering", {
  p = learner$train(task)$predict(task)

  # filter to 3 observations
  p$filter(c(20, 37, 42))
  expect_prediction_cmprsk(p)
  expect_setequal(p$data$row_ids, c(20, 37, 42))
  expect_length(p$truth, 3)

  # edge case: filter to 1 observation
  p$filter(20)
  expect_prediction_cmprsk(p)
  expect_setequal(p$data$row_ids, 20)
  expect_length(p$truth, 1)

  # filter to 0 observations using non-existent (positive) id
  p$filter(42)
  expect_prediction_cmprsk(p)
})
