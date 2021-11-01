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

test_that("as_task_dens", {
  expect_task_dens(as_task_dens(data.frame(times = 1:100)))

  t1 = tsk("precip")
  t2 = as_task_dens(t1, clone = TRUE)
  expect_task_dens(t2)
  t1$filter(1:10)
  expect_equal(t1$nrow, 10L)
  expect_equal(t2$nrow, 70L)

  t1 = tsk("precip")
  t2 = as_task_dens(t1, clone = FALSE)
  expect_task_dens(t2)
  t1$filter(1:10)
  expect_equal(t1$nrow, 10L)
  expect_equal(t2$nrow, 10L)
})
