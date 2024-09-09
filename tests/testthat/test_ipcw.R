test_that("PipeOpTaskSurvClassifIPCW", {
  task = tsk("lung")

  # imitate train/test split manually
  test_ids = c(2, 10, 107)
  train_ids = setdiff(task$row_ids, test_ids)
  test_task = task$clone()$filter(rows = test_ids)
  train_task = task$clone()$filter(rows = train_ids)
  expect_equal(test_task$row_ids, test_ids)
  expect_equal(train_task$row_ids, train_ids)

  po_ipcw = mlr3pipelines::po("trafotask_survclassif_IPCW")
  expect_class(po_ipcw, c("PipeOp", "PipeOpTaskSurvClassifIPCW"))
  # don't allow NULL `cutoff_time`
  expect_error(po_ipcw$train(list(train_task)), "not 'NULL'")
  # `cutoff_time` should be less than the max event time
  po_ipcw$param_set$set_values(cutoff_time = 3000)
  expect_error(po_ipcw$train(list(train_task)), "cutoff_time < max_event_time is not TRUE")
  po_ipcw$param_set$set_values(cutoff_time = 300)

  res = po_ipcw$train(list(train_task))

  # no output data during training
  expect_null(res[["data"]])
  # classification task
  output_task = res[[1L]]
  expect_task_classif(output_task)
  expect_equal(output_task$col_roles$weight, "ipc_weights")
  expect_equal(output_task$positive, "1")
  expect_equal(output_task$target_names, "status")
  expect_equal(output_task$nrow, train_task$nrow) # same observations

  # check: is status target correct? (0 before cutoff)
  # check: do `output_task$weights` make sense? are 0 the ones that should be 0?

  res = po_ipcw$predict(list(test_task))
  pred_task = res[[1L]]

  expect_task_classif(pred_task)
  # check status?

  # (row_ids, times, status, cutoff_time) are correct?
  data = res[[2L]]
  expect_list(data, len = 4)
  expect_true(length(data$row_ids) == length(test_ids))
  expect_equal(names(data), c("row_ids", "times", "status", "cutoff_time"))
})
