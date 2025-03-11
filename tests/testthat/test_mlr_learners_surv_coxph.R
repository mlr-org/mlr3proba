test_that("autotest", {
  with_seed(42L, {
    learner = lrn("surv.coxph")
    expect_learner(learner)
    result = run_autotest(learner, check_replicable = FALSE)
    expect_true(result, info = result$error)
  })
})

test_that("missing", {
  learner = lrn("surv.coxph")
  lung = survival::lung
  lung$status = lung$status - 1
  task = mlr3proba::TaskSurv$new("lung", backend = lung, time = "time", event = "status")
  expect_error(learner$predict(task), "has missing values")
})
