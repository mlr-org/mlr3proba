expect_task_dens = function(task) {
  expect_is(task, "TaskDens")
  expect_task(task)
  expect_is(task$data(), "data.table")
  expect_equal(task$ncol, 1L)

  f = task$formula()
  expect_formula(f)
}

expect_prediction_dens = function(p) {
  expect_prediction(p)
  expect_is(p, "PredictionDens")
}

expect_task_surv = function(task) {
  expect_is(task, "TaskSurv")
  expect_task(task)
  expect_task_supervised(task)
  expect_is(task$truth(), "Surv")

  f = task$formula()
  expect_formula(f)
  expect_set_equal(mlr3misc::extract_vars(f)$lhs, task$target_names)
  #  expect_is(task$survfit(), "survfit")
}

expect_prediction_surv = function(p) {
  checkmate::expect_r6(p, "Prediction", public = c("row_ids", "truth", "predict_types",
                                                   "response", "distr", "lp", "crank"))
  #testthat::expect_output(print(p), "^<Prediction")
  checkmate::expect_data_table(data.table::as.data.table(p), nrows  = length(p$row_ids))
  checkmate::expect_atomic_vector(p$missing)
  if ("distr" %in% p$predict_types) {
    checkmate::expect_class(p$distr, "VectorDistribution")
  }
  expect_is(p, "PredictionSurv")
}
