context("TaskSurv")

test_that("Task duplicates rows", {
  task = tsk("lung")
  expect_task_surv(task)
  expect_set_equal(extract_vars(task$formula())$rhs, ".")
})
