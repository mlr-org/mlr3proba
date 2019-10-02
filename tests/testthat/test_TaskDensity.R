# context("TaskDensity")
#
# test_that("Basic ops on BostonHousing task", {
#   task = tsk("precip")
#   expect_task(task)
#   expect_task_supervised(task)
#   expect_task_density(task)
#   expect_equal(task$target_names, "precip")
#
#   f = task$formula()
#   expect_class(f, "formula")
#   # expect_set_equal(attr(terms(f), "term.labels"), task$feature_names)
# })
#
# test_that("TaskDensity: 0 feature task", {
#   b = as_data_backend(data.table::data.table(y = runif(20)))
#   task = TaskDensity$new(id = "zero_feat_task", b, target = "y")
#   expect_output(print(task))
#   b = task$backend
#   expect_backend(b)
#   expect_task(task)
#   expect_task_supervised(task)
#   expect_task_density(task)
#   expect_data_table(task$data(), ncols = 1L)
#
#   # lrn = lrn("density.kde")
#   # p = lrn$train(task)$predict(task)
#   # expect_prediction(p)
#   # expect_number(e$performance)
# })
