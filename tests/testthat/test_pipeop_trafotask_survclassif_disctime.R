test_that("PipeOpTaskSurvClassifDiscTime", {
  task = tsk("lung")

  # imitate train/test split manually
  test_ids = c(2, 10, 107)
  train_ids = setdiff(task$row_ids, test_ids)
  test_task = task$clone()$filter(rows = test_ids)
  train_task = task$clone()$filter(rows = train_ids)
  expect_equal(test_task$row_ids, test_ids)
  expect_equal(train_task$row_ids, train_ids)

  po_disc = mlr3pipelines::po("trafotask_survclassif_disctime", cut = 4)
  expect_class(po_disc, c("PipeOp", "PipeOpTaskSurvClassifDiscTime"))

  res = po_disc$train(list(train_task))

  # 0 is added
  time_cuts = po_disc$state$cut
  expect_numeric(time_cuts, len = 5, lower = 0)
  # no transformed data during training
  expect_data_table(res[["transformed_data"]], nrows = 0, ncols = 0)
  # classification task
  output_task = res[[1L]]
  expect_task_classif(output_task)
  expect_equal(output_task$col_roles$original_ids, "id")
  expect_equal(output_task$positive, "1")
  expect_equal(output_task$target_names, "ped_status")
  # new column added to the task
  expect_equal("tend", setdiff(output_task$feature_names, task$feature_names))
  # not all observations have events on the last (4th) interval
  expect_lt(output_task$nrow, task$nrow * 4)

  res = po_disc$predict(list(test_task))
  pred_task = res[[1L]]

  expect_task_classif(pred_task)
  # every test observation will have one row per interval for prediction
  expect_equal(pred_task$nrow, test_task$nrow * 4)
  # `tend` matches the cut time points (excluding 0 time point)
  tends = pred_task$data(cols = "tend")[[1L]]
  expect_equal(sort(unique(tends)), time_cuts[2:5])
  # test row ids are correct
  expect_equal(pred_task$col_roles$original_ids, "id")
  original_ids = pred_task$data(cols = "id")[[1L]]
  correct_ids = rep(test_ids, each = 4)
  expect_equal(original_ids, correct_ids)

  transformed_data = res[["transformed_data"]]
  # test rows ids are correct
  expect_equal(transformed_data$id, correct_ids)
  # check columns in the transformed data.table
  expect_equal(sort(c("id", "ped_status", "obs_times", pred_task$feature_names)),
               sort(colnames(transformed_data)))

  # `ped_status` per interval and per observation is correct
  # before observed time ("obs_times"), "ped_status" = 0
  expect_equal(as.character(unique(transformed_data[tend < obs_times, ped_status])),
               "0")
  times = test_task$times() # observed times
  status = as.character(test_task$status())
  # after observed time, "ped_status" must be the same as "status"
  td = transformed_data[tend > obs_times]
  expect_equal(as.character(unique(td[id == test_ids[1], ped_status])), status[1])
  expect_equal(as.character(unique(td[id == test_ids[2], ped_status])), status[2])
  expect_equal(as.character(unique(td[id == test_ids[3], ped_status])), status[3])
})
