test_that("mlr_task_generators_simdens", {
  gen = mlr_task_generators$get("simdens")
  task = gen$generate(10)
  expect_task_dens(task)
})
