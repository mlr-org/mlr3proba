context("benchmark")

test_that("benchmark works", {
  learners = lapply(c("surv.kaplan", "surv.nelson"), lrn)
  task = tsk("rats")
  bmr = benchmark(benchmark_grid(task, learners, rsmp("cv", folds = 3)))
  expect_benchmark_result(bmr)
})
