skip_if_not_installed("mlr3proba")
require_namespaces("mlr3proba")
task = tsk("rats")

test_that("autoplot.TaskSurv", {
  p = autoplot(task, type = "target")
  expect_true(is.ggplot(p))

  p = autoplot(task, type = "pairs")
  expect_s3_class(p, "ggmatrix")

  p = autoplot(task, type = "duo")
  expect_s3_class(p, "ggmatrix")
})

test_that("autoplot.PredictionSurv", {
  learner = suppressWarnings(mlr3::lrn("surv.coxph")$train(task))
  prediction = learner$predict(task)

  p = autoplot(prediction, type = "calib")
  expect_true(is.ggplot(p))

  p = autoplot(prediction, type = "dcalib", cuts = 4)
  expect_true(is.ggplot(p))

  p = suppressWarnings(autoplot(prediction, type = "scalib", time = 95))
  expect_true(is.ggplot(p))

  p = autoplot(prediction, type = "isd", row_ids = sample(task$row_ids, size = 5))
  expect_true(is.ggplot(p))
})

test_that("autoplot.TaskDens", {
  skip_if_not_installed("mlr3proba")
  require_namespaces("mlr3proba")

  task = tsk("precip")

  p = autoplot(task, type = "dens")
  expect_true(is.ggplot(p))

  p = autoplot(task, type = "freq")
  expect_true(is.ggplot(p))

  p = autoplot(task, type = "overlay")
  expect_true(is.ggplot(p))

  p = autoplot(task, type = "freqpoly")
  expect_true(is.ggplot(p))
})
