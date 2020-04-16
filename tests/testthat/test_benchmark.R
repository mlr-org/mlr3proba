context("benchmark")

test_that("benchmark works", {
  learners = lapply(c("surv.kaplan", "surv.coxph"), lrn)
  task = tsk("rats")
  bmr = suppressWarnings(benchmark(benchmark_grid(task, learners, rsmp("cv", folds = 3))))
  expect_benchmark_result(bmr)
})
