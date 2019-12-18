context("single-step")

test_that("single-step", {
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
  # expect_set_equal(p$row_id, 3001:3343)
})
