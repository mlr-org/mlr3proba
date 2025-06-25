test_that("PipeOpTaskSurvRegrPEM", {
  task = tsk("lung")

  # imitate train/test split manually
  test_ids = c(2, 10, 107)
  train_ids = setdiff(task$row_ids, test_ids)
  test_task = task$clone()$filter(rows = test_ids)
  train_task = task$clone()$filter(rows = train_ids)
  expect_equal(test_task$row_ids, test_ids)
  expect_equal(train_task$row_ids, train_ids)

  po_pem = po("trafotask_survregr_pem", cut = 4)
  expect_class(po_pem, c("PipeOp", "PipeOpTaskSurvRegrPEM"))

  res = po_pem$train(list(train_task))
  # 0 is added
  time_cuts = po_pem$state$cut
  expect_numeric(time_cuts, len = 5, lower = 0)
  # no transformed data during training
  expect_data_table(res[["transformed_data"]], nrows = 0, ncols = 0)
  # regression task
  output_task = res[[1L]]
  expect_task_regr(output_task)
  expect_equal(output_task$col_roles$original_ids, "id")
  expect_equal(output_task$target_names, "pem_status")

  # offset present and correctly assigned
  expect_equal(output_task$properties, "offset")
  expect_equal(output_task$col_roles$offset, "offset")
  # new column added to the task + offset is not a feature
  expect_equal("tend", setdiff(output_task$feature_names, task$feature_names))
  # not all observations have events on the last (4th) interval
  expect_lt(output_task$nrow, task$nrow * 4)

  res = po_pem$predict(list(test_task))

  # check: output is regression task
  pred_task = res[[1L]]
  expect_task_regr(pred_task)
  # every test observation will have one row per interval for prediction
  expect_equal(pred_task$nrow, test_task$nrow * 4)
  # `tend` matches the cut time points (excluding 0 time point)
  tends = pred_task$data(cols = "tend")[[1L]]
  expect_setequal(unique(tends), time_cuts[2:5])
  # original row ids are correct
  expect_equal(pred_task$col_roles$original_ids, "id")
  original_ids = pred_task$data(cols = "id")[[1L]]
  correct_ids = rep(test_ids, each = 4)
  expect_equal(original_ids, correct_ids)

  transformed_data = res[["transformed_data"]]
  # check columns in the transformed data.table
  expect_setequal(colnames(transformed_data),
                  c("id", "pem_status", "obs_times", "tend", "offset"))
  # `id`s are correct
  expect_equal(transformed_data$id, correct_ids)
  # `pem_status` is the same
  expect_equal(as.character(transformed_data$pem_status),
               as.character(pred_task$truth()))
  # `obs_times` are correct
  times = test_task$times() # observed times
  expect_setequal(unique(transformed_data$obs_times), times)
  # `tends` are correct
  expect_setequal(unique(transformed_data$tend), time_cuts[2:5])

  # `pem_status` per interval and per observation is correct
  # before observed time ("obs_times"), "pem_status" = 0
  expect_equal(as.character(unique(transformed_data[tend < obs_times, pem_status])), "0")

  # after observed time, "pem_status" must be the same as "status"
  status = as.character(test_task$status())
  td = transformed_data[tend > obs_times]
  expect_equal(as.character(unique(td[id == test_ids[1], pem_status])), status[1])
  expect_equal(as.character(unique(td[id == test_ids[2], pem_status])), status[2])
  expect_equal(as.character(unique(td[id == test_ids[3], pem_status])), status[3])
})
