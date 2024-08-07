test_that("autotest", {
  set.seed(1L)
  learner = lrn("dens.kde")
  expect_learner(learner)
  # Skip due to long execution time (distr6 needs to be improved)
  skip_on_os(os = c("windows", "mac", "linux", "solaris"))
  result = run_autotest(learner, check_replicable = FALSE)
  expect_true(result, info = result$error)
})

x = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6)
task = TaskDens$new(id = "a", x)
lrn = lrn("dens.kde", bandwidth = 0.5, kernel = "Epan")
lrn$train(task)

test_that("pdf", {
  d = stats::density(x, kernel = "epan", bw = 0.5)
  task_test = TaskDens$new(id = "a", d$x[20L])
  expect_equal(round(d$y[20L], 2), round(lrn$predict(task_test)$pdf, 2))
})
