# context("PredictionDensity")
#
# task = tsk("precip")
# lrn = lrn("density.kde")
#
# test_that("Construction", {
#   p = PredictionDensity$new(task, task$row_ids, task$truth())
#   expect_prediction_density(p)
# })
#
# test_that("Internally constructed Prediction", {
#   p = lrn$train(task)$predict(task)
#   expect_prediction_density(p)
# })
#
# test_that("c", {
#   resampling = rsmp("cv", folds = 3)
#   rr = resample(task, lrn, resampling)
#
#   preds = rr$predictions
#
#   pred = do.call(c, preds)
#   expect_prediction_density(pred)
#
#   dt = as.data.table(pred)
#   expect_data_table(dt, nrows = task$nrow, ncols = 3L, any.missing = FALSE)
# })
