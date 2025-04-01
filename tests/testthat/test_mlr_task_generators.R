test_that("simdens", {
  gen = tgen("simdens")
  task = gen$generate(10L)
  expect_task_dens(task)
})

skip_if_not_installed("simsurv")
test_that("simsurv", {
  gen = tgen("simsurv")
  task = gen$generate(50L)
  expect_task_surv(task)
})

skip_if_not_installed("coxed")
test_that("coxed", {
  gen = tgen("coxed", censor = 0.5)
  expect_equal(gen$param_set$values$type, "none")
  task = gen$generate(20L)
  expect_task_surv(task)
  expect_equal(task$cens_type, "right")

  gen$param_set$set_values(type = "tvbeta")
  expect_equal(gen$param_set$values$type, "tvbeta")
  task = gen$generate(20L)
  expect_task_surv(task)
  expect_equal(task$cens_type, "right")
})
