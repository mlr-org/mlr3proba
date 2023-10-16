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

  p = reshape_distr_to_3d(p)
  expect_prediction_surv(p)
})

lrn = lrn("surv.kaplan")

test_that("c", {
  set.seed(1)
  resampling = rsmp("cv", folds = 2)
  rr = resample(task, lrn, resampling)

  preds = rr$predictions()

  # combining survival matrices
  # same number of time points (columns) but different values
  distr1 = preds[[1]]$data$distr
  distr2 = preds[[2]]$data$distr
  times1 = as.integer(colnames(distr1))
  times2 = as.integer(colnames(distr2))
  expect_true(length(times1) == length(times2))
  expect_false(all(times1 == times2))

  pred = do.call(c, preds)
  expect_prediction_surv(pred)
  surv_mat = pred$data$distr
  expect_class(surv_mat, "matrix")
  # check that time points are properly combined
  times = as.integer(colnames(surv_mat))
  expect_true(all(times == sort(union(times1, times2), decreasing = F)))

  # data.table conversion
  dt = as.data.table(pred)
  expect_data_table(dt, nrows = task$nrow, ncols = 5L, any.missing = FALSE)

  # different number of time points
  # add extra time point on the 2nd prediction object
  preds2 = rr$predictions()
  preds2[[2]]$data$distr = cbind(distr2,
    matrix(data = rep(0.3, 10), ncol = 1, dimnames = list(NULL, 108)))
  distr1 = preds2[[1]]$data$distr
  distr2 = preds2[[2]]$data$distr
  times1 = as.integer(colnames(distr1))
  times2 = as.integer(colnames(distr2))
  expect_false(length(times1) == length(times2))

  pred2 = do.call(c, preds2)
  expect_prediction_surv(pred2)
  surv_mat2 = pred2$data$distr
  expect_class(surv_mat2, "matrix")
  # check that time points are properly combined
  times = as.integer(colnames(surv_mat2))
  expect_true(all(times == sort(union(times1, times2), decreasing = F)))

  # combining survival arrays
  arr_preds = mlr3misc::map(preds2, reshape_distr_to_3d)
  arr_pred = do.call(c, arr_preds)
  expect_prediction_surv(arr_pred)
  expect_class(arr_pred$data$distr, "array")
  expect_class(arr_pred$distr, "Arrdist")
  # check that time points are properly combined
  times1 = as.integer(colnames(arr_preds[[1]]$data$distr))
  times2 = as.integer(colnames(arr_preds[[2]]$data$distr))
  times  = as.integer(colnames(arr_pred$data$distr))
  expect_equal(as.integer(colnames(arr_pred$data$distr)),
    sort(union(times1, times2), decreasing = F))

  p1 = lrn("surv.kaplan")$train(task)$predict(task)
  p2 = suppressWarnings(lrn("surv.coxph")$train(task))$predict(task)
  expect_error(c(p1, p2), "Cannot combine")

  # combining predictions with exactly the same time points
  p1 = lrn("surv.kaplan")$train(task)$predict(task)
  p2 = p1$clone()
  expect_equal(length(c(p1, p2, keep_duplicates = TRUE)$row_ids), 40)
  expect_equal(length(c(p1, p2, keep_duplicates = FALSE)$row_ids), 20)
  preds = list(p1, p2)
  p12 = do.call(c, preds)
  expect_class(p12$data$distr, "matrix") # combination is a matrix
  expect_equal(colnames(p12$data$distr), colnames(p1$data$distr)) # same time points

  arr_p1 = reshape_distr_to_3d(p1)
  arr_p2 = reshape_distr_to_3d(p2)
  arr_preds = list(arr_p1, arr_p2)
  arr_pred = do.call(c, arr_preds)
  expect_class(arr_pred$data$distr, "array") # combination is an array
  expect_equal(colnames(arr_pred$data$distr), colnames(arr_p1$data$distr)) # same time points

  # combining distr6::Distribution objects of the same type
  # Matdist
  p1$data$distr = p1$distr
  p2$data$distr = p2$distr
  preds2 = list(p1, p2)
  pred2 = do.call(c, preds2)
  expect_class(pred2$data$distr, "matrix")
  expect_true(all(pred2$data$distr == p12$data$distr))

  # Arrdist
  arr_p1$data$distr = arr_p1$distr
  arr_p2$data$distr = arr_p2$distr
  arr_preds2 = list(arr_p1, arr_p2)
  arr_pred2 = do.call(c, arr_preds2)
  expect_class(arr_pred2$data$distr, "array")
  expect_true(all(arr_pred2$data$distr == arr_pred$data$distr))

  # combining distr6::Distribution objects of different types
  mix_preds = list(p1, arr_p2) # Matdist and Arrdist
  expect_error(supressWarnings(do.call(c, mix_preds)))

  # combine survival matrix and Matdist (matrix converts to a Matdist)
  p1 = lrn("surv.kaplan")$train(task)$predict(task)
  p2 = p1$clone()
  p2$data$distr = p2$distr
  preds = list(p1, p2)
  expect_prediction_surv(do.call(c, preds))

  # combine survival array and Matdist (array converts to an Arrdist)
  preds = list(reshape_distr_to_3d(p1), p2)
  expect_error(do.call(c, preds))

  # combine survival matrix and Arrdist (matrix converts to a Matdist)
  p2 = p1$clone()
  p2 = reshape_distr_to_3d(p2)
  p2$data$distr = p2$distr
  preds = list(p1, p2)
  expect_error(supressWarnings(do.call(c, preds)))

  # combine survival array and Arrdist (array converts to an Arrdist)
  preds = list(reshape_distr_to_3d(p1), p2)
  expect_prediction_surv(do.call(c, preds))

  # combine survival matrix and array
  p2 = p1$clone()
  p2 = reshape_distr_to_3d(p2)
  expect_array(p2$data$distr, d = 3)
  # add extra time point in the survival matrix
  p1$data$distr = cbind(p1$data$distr,
    matrix(data = rep(0.3, 20), ncol = 1, dimnames = list(NULL, 108)))
  expect_matrix(p1$data$distr, nrows = 20)
  preds = list(p1, p2)
  pred = do.call(c, preds)
  expect_prediction_surv(pred)
  expect_true("108" %in% colnames(pred$data$distr)) # time point was added for all observations
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
  p2 = reshape_distr_to_3d(p) # survival array distr

  p$filter(c(20, 37, 42))
  p2$filter(c(20, 37, 42))
  expect_prediction_surv(p)
  expect_prediction_surv(p2)

  expect_set_equal(p$data$row_ids, c(20, 37, 42))
  expect_set_equal(p2$data$row_ids, c(20, 37, 42))
  expect_numeric(p$data$crank, any.missing = FALSE, len = 3)
  expect_numeric(p2$data$crank, any.missing = FALSE, len = 3)
  expect_numeric(p$data$lp, any.missing = FALSE, len = 3)
  expect_numeric(p2$data$lp, any.missing = FALSE, len = 3)
  expect_matrix(p$data$distr, nrows = 3)
  expect_array(p2$data$distr, d = 3)
  expect_equal(nrow(p2$data$distr), 3)
})
