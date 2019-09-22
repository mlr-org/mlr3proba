context("mlr_measures")

test_that("mlr_measures", {
  task = mlr_tasks$get("lung")
  keys = mlr_measures$keys("^surv")

  for (key in keys) {
    m = mlr_measures$get(key)
    expect_measure(m)

    perf = mlr_learners$get("surv.rpart")$train(task)$predict(task)$score()
    expect_number(perf, na.ok = "na_score" %in% m$properties)
  }
})
