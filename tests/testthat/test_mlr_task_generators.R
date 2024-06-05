test_that("mlr_task_generators_simdens", {
  gen = tgen("simdens")
  task = gen$generate(10)
  expect_task_dens(task)
})

skip_if_not_installed("simsurv")
test_that("mlr_task_generators_simsurv", {
  gen = tgen("simsurv")
  task = gen$generate(50)
  expect_task_surv(task)
})

skip_if_not_installed("coxed")
test_that("mlr_task_generators_coxed", {
  gen = tgen("coxed", censor = 0.5)
  task = gen$generate(20)
  expect_task_surv(task)
  expect_equal(task$censtype, "right")

  gen$param_set$set_values(type = "tvbeta")
  task = gen$generate(20)
  expect_task_surv(task)
  expect_equal(task$censtype, "right")

  gen$param_set$set_values(type = "tvc")
  task = gen$generate(20)
  expect_task_surv(task)
  expect_equal(task$censtype, "counting")
  task$formula()
})
