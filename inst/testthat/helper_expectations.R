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
  expect_r6(p, "Prediction", public = c("row_ids", "truth", "predict_types",
                                                   "response", "distr", "lp", "crank"))
  expect_data_table(as.data.table(p), nrows = length(p$row_ids))
  expect_atomic_vector(p$missing)
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

expect_prediction_cmprsk = function(p) {
  expect_r6(p, "Prediction", public = c("row_ids", "truth", "predict_types", "cif"))
  n_obs = length(p$row_ids)

  if (n_obs > 0) {
    # convert to data.table
    tab = as.data.table(p)

    expect_data_table(tab, nrows = n_obs, ncols = 4, col.names = "named")
    expect_set_equal(names(tab), c("row_ids", "time", "event", "CIF"))
    expect_list(tab$CIF, len = n_obs, any.missing = FALSE, null.ok = FALSE)
    # Assert that every element of tab$CIF (i.e. corresponding to each observation)
    # is a list containing numeric CIF vectors, one per competing risk
    n_cmp_events = length(attr(p$truth, "states"))
    expect_true(all(sapply(tab$CIF, function(x) {
      check_list(x, len = n_cmp_events) && all(sapply(x, is.numeric))
    })))

    expect_atomic_vector(p$missing)

    # check that CIF data list makes sense
    if ("cif" %in% p$predict_types && !is.null(p$data$cif)) {
      check_prediction_data(p$data, n_rows = n_obs, n_cmp_events = n_cmp_events)

      # TODO: check for `survdistr` class when `p$cif` returns that
    }
  }

  expect_class(p, "PredictionCompRisks")
}
