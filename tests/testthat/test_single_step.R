context("single-step")

test_that("single-step surv", {
  task = mlr_tasks$get("unemployment")
  learner = mlr_learners$get("surv.kaplan")

  train_task = task$clone()$filter(1:3000)
  learner$train(train_task)
  newdata = remove_named(task$clone()$filter(3001:3343)$data(), task$target_names)
  p = learner$predict_newdata(task = train_task, newdata = newdata)

  p = as.data.table(p)
  expect_data_table(p, nrow = 343)
  expect_true(allMissing(p$time))
  expect_true(allMissing(p$status))
})


test_that("single-step dens", {
  task = tsk("faithful")
  learner = lrn("dens.kde")
  train_task = task$clone()$filter(1:150)
  learner$train(train_task)
  newdata = task$clone()$filter(151:task$nrow)$data()
  p = learner$predict_newdata(task = train_task, newdata = newdata)
  expect_prediction(p)

  tab = as.data.table(p)
  expect_data_table(tab, nrow = task$nrow - 150)
})
