test_that("autotest", {
  set.seed(1L)
  learner = lrn("dens.kde")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})

test_that("pdf", {
  x = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6)
  task = TaskDens$new(id = "a", x)
  lrn = lrn("dens.kde", bandwidth = 0.5, kernel = "Epan")
  lrn$train(task)

  d = stats::density(x, kernel = "epan", bw = 0.5)
  task_test = TaskDens$new(id = "a", d$x[20L])
  expect_equal(round(d$y[20L], 2), round(lrn$predict(task_test)$pdf, 2))
})
