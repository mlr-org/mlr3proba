expect_task_dens = function(task) {
  expect_class(task, "TaskDens")
  expect_task(task)
  expect_class(task$data(), "data.table")
  expect_identical(task$ncol, 1L)

  f = task$formula()
  expect_formula(f)
}

expect_prediction_dens = function(p) {
  expect_prediction(p)
  expect_class(p, "PredictionDens")
}

expect_task_surv = function(task) {
  expect_class(task, "TaskSurv")
  expect_task(task)
  expect_task_supervised(task)
  expect_class(task$truth(), "Surv")

  f = task$formula()
  expect_formula(f)
  expect_setequal(extract_vars(f)$lhs, task$target_names)
  expect_class(task$kaplan(), "survfit")
}

expect_prediction_surv = function(p) {
  checkmate::expect_r6(p, "Prediction", public = c("row_ids", "truth", "predict_types",
                                                   "response", "distr", "lp", "crank"))
  checkmate::expect_data_table(data.table::as.data.table(p), nrows  = length(p$row_ids))
  checkmate::expect_atomic_vector(p$missing)
  if ("distr" %in% p$predict_types && !is.null(p$distr)) {
    expect_true(class(p$distr)[[1L]] %in% c("VectorDistribution", "Matdist", "Arrdist", "WeightedDiscrete"))
  }
  expect_true(inherits(p, "PredictionSurv"))
}

expect_task_cmprsk = function(task) {
  expect_class(task, "TaskCompRisks")
  expect_task(task)
  expect_task_supervised(task)
  expect_class(task$truth(), "Surv")

  f = task$formula()
  expect_formula(f)
  expect_setequal(extract_vars(f)$lhs, task$target_names)
  expect_class(task$aalen_johansen(), "survfit")
}
