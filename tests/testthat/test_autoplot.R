skip_if_not_installed("ggplot2")
skip_if_not_installed("GGally")
skip_if_not_installed("survminer")
skip_if_not_installed("polspline")

test_that("autoplot functions work", {
  # survival tasks
  task = tsk("rats")
  learner = suppressWarnings(lrn("surv.coxph")$train(task))
  prediction = learner$predict(task)

  # TaskSurv
  p = autoplot(task, type = "target")
  expect_true(is_ggplot(p))

  p = autoplot(task, type = "pairs")
  expect_s3_class(p, "ggmatrix")

  p = autoplot(task, type = "duo")
  expect_s3_class(p, "ggmatrix")

  # LearnerSurvCoxPH
  p = autoplot(learner)
  expect_true(is_ggplot(p))

  # PredictionSurv
  p = autoplot(prediction, type = "calib")
  expect_true(is_ggplot(p))

  p = autoplot(prediction, type = "dcalib", cuts = 4)
  expect_true(is_ggplot(p))

  p = suppressWarnings(autoplot(prediction, type = "scalib", time = 95))
  expect_true(is_ggplot(p))

  p = autoplot(prediction, type = "isd", row_ids = sample(task$row_ids, size = 5))
  expect_true(is_ggplot(p))

  # TaskDens
  task_dens = tsk("precip")

  p = autoplot(task_dens, type = "dens")
  expect_true(is_ggplot(p))

  p = autoplot(task_dens, type = "freq")
  expect_true(is_ggplot(p))

  p = autoplot(task_dens, type = "overlay")
  expect_true(is_ggplot(p))

  p = autoplot(task_dens, type = "freqpoly")
  expect_true(is_ggplot(p))
})
