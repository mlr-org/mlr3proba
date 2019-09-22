context("benchmark")

test_that("benchmark works", {
  learners = lapply(c("surv.coxph", "surv.rpart", "surv.ranger"), lrn)
  task = tsk("rats")
  bmr = benchmark(benchmark_grid(task, learners, rsmp("cv", folds = 3)))
  expect_benchmark_result(bmr)
})
