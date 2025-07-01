test_that("mlr_measures", {
  task = TaskGeneratorSimdens$new()$generate(20L)
  m = msr("dens.logloss")
  expect_measure(m)
  perf = lrn("dens.hist")$train(task)$predict(task)$score(m)
  expect_number(perf)
})
