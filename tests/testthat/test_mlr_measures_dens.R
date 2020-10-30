task = TaskGeneratorSimdens$new()$generate(20)

test_that("mlr_measures", {
  keys = mlr_measures$keys("^dens")

  for (key in keys) {

    m = mlr_measures$get(key)

    expect_measure(m)

    perf = mlr_learners$get("dens.hist")$train(task)$predict(task)$score()
    expect_number(perf, na.ok = "na_score" %in% m$properties)
  }
})
