test_that("PipeOpTaskSurvClassifIPCW", {

  task = tsk("rats")
  po = PipeOpTaskfSurvClassifIPCW$new()
  l = lrn("classif.gam")

  pipe = po %>>% l
  pipe$train(task)
  pred1 = pipe$predict(task)$classif.gam.output
  expect_prediction_classif(pred1)
})
