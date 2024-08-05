skip_if_not_installed("mlr3extralearners")
test_that("PipeOpTaskSurvClassifIPCW", {

  task = tsk("rats")
  po = po("trafotask_survclassif_IPCW", cutoff_time = 50)
  l = lrn("classif.gam")

  pipe = po %>>% l
  suppressWarnings(pipe$train(task))
  pred1 = pipe$predict(task)$classif.gam.output
  expect_prediction_classif(pred1)


  po = po("trafotask_survclassif_IPCW", cutoff_time = 75)

  pipe2 = po %>>% l
  suppressWarnings(pipe2$train(task))
  pred2 = pipe2$predict(task)$classif.gam.output

  testthat::expect_true(all(pred1$prob != pred2$prob))
})
