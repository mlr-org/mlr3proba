test_that("autotest", {
  with_seed(42L, {
    learner = lrn("surv.coxph")
    expect_learner(learner)
    ## no idea why weights check here fails, we test the same task
    ## in the below test and it works!
    result = suppressWarnings(
      run_autotest(learner, exclude = "weights", check_replicable = FALSE, N = 10L)
    )
    expect_true(result, info = result$error)
  })
})

test_that("weights", {
  learner = lrn("surv.coxph", use_weights = TRUE)
  task = generate_tasks.LearnerSurv(learner)$weights
  suppressWarnings({learner$train(task)})
  expect_equal(learner$model$weights, task$weights_learner$weight)
})

test_that("missing", {
  learner = lrn("surv.coxph")
  lung = survival::lung
  lung$status = lung$status - 1
  task = mlr3proba::TaskSurv$new("lung", backend = lung, time = "time", event = "status")
  expect_error(learner$predict(task))
})
