context("TaskDens")

test_that("Basic ops on BostonHousing task", {
  task = tsk("precip")
  expect_task(task)
  expect_task_dens(task)

  f = task$formula()
  expect_class(f, "formula")
})

test_that("TaskDens: 0 feature task", {
  task = TaskDens$new(id = "zero_feat_task", runif(20))
  expect_output(print(task))
  b = task$backend
  expect_backend(b)
  expect_task(task)
  expect_task_dens(task)
  expect_data_table(task$data(), ncols = 1L)
})
