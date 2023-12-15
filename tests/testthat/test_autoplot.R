
test_that("autoplot.PredictionSurv", {
  skip_if_not_installed("mlr3proba")
  require_namespaces("mlr3proba")

  task = mlr3::tsk("rats")$filter(1:100)
  learner = suppressWarnings(mlr3::lrn("surv.coxph")$train(task))
  prediction = learner$predict(task)

  p = autoplot(prediction, type = "calib", task = task)
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("predictionsurv_calib", p)

  p = autoplot(prediction, type = "dcalib")
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("predictionsurv_dcalib", p)

  p = autoplot(prediction, type = "preds")
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("predictionsurv_preds", p)
})

test_that("autoplot.TaskDens", {
  skip_if_not_installed("mlr3proba")
  require_namespaces("mlr3proba")
  task = mlr3::tsk("precip")

  p = autoplot(task, type = "dens")
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("taskdens_dens", p)

  p = autoplot(task, type = "freq")
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("taskdens_freq", p)

  p = autoplot(task, type = "overlay")
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("taskdens_overlay", p)

  p = autoplot(task, type = "freqpoly")
  expect_true(is.ggplot(p))
  vdiffr::expect_doppelganger("taskdens_freqpoly", p)
})

test_that("autoplot.TaskSurv", {
  skip_if_not_installed("mlr3proba")

  require_namespaces("mlr3proba")
  task = mlr3::tsk("rats")

  p = autoplot(task, type = "target")
  expect_true(is.ggplot(p))
  # vdiffr::expect_doppelganger("tasksurv_target", p)

  p = autoplot(task, type = "pairs")
  expect_s3_class(p, "ggmatrix")
  # vdiffr::expect_doppelganger("tasksurv_ggmatrix", p)

  p = autoplot(task, type = "duo")
  expect_s3_class(p, "ggmatrix")
  # vdiffr::expect_doppelganger("tasksurv_duo", p)
})
