context("TaskDens")

test_that("Basic ops on BostonHousing task", {
  task = tsk("precip")
  expect_task(task)
  expect_task_dens(task)
  expect_equal(task$target_names, "precip")

  f = task$formula()
  expect_class(f, "formula")
  # expect_set_equal(attr(terms(f), "term.labels"), task$feature_names)
})

test_that("TaskDens: 0 feature task", {
  b = as_data_backend(data.table::data.table(y = runif(20)))
  task = TaskDens$new(id = "zero_feat_task", b, target = "y")
  expect_output(print(task))
  b = task$backend
  expect_backend(b)
  expect_task(task)
  expect_task_dens(task)
  expect_data_table(task$data(), ncols = 1L)
})
