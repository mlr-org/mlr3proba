context("dens.kde")

test_that("autotest", {
  set.seed(1)
  learner = lrn("dens.kde")
  expect_learner(learner)
  result = run_autotest(learner, check_replicable = FALSE)
  expect_true(result, info = result$error)
})


data = data.frame("A" = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4, 1.6))
task = TaskDens$new(id = "a", data, target = "A")
lrn = lrn("dens.kde", bandwidth = 0.1, kernel = "Norm")
lrn$train(task)

test_that("pdf", {
  set.seed(1)
  task_test = TaskDens$new(id = "a", data.frame(a = c(0.5)), target = "a")
  expect_equal(round(lrn$predict(task_test)$pdf, 3), c(0.616))
  task_test = TaskDens$new(id = "a", data.frame(a = c(0.5, 1.5, 0)), target = "a")
  expect_equal(round(lrn$predict(task_test)$pdf, 3), c(0.616, 0.610, 0.068))
  d = stats::density(data$A, bw = 0.1)
  task_test = TaskDens$new(id = "a", data.frame(a = d$x[20]), target = "a")
  expect_equal(round(d$y[20], 2), round(lrn$predict(task_test)$pdf, 2))
})

test_that("bw", {
  set.seed(1)
  lrn = lrn("dens.kde")
  lrn$train(task)
  d = stats::density(data$A, bw = 0.2908909)
  task_test = TaskDens$new(id = "a", data.frame(a = d$x[20]), target = "a")
  expect_equal(round(d$y[20], 3), round(lrn$predict(task_test)$pdf, 3))
})
