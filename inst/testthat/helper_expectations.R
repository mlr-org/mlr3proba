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

# see mlr3pipelines
expect_pipeop = function(po) {
  label = sprintf("pipeop '%s'", po$id)
  expect_class(po, "PipeOp", label = label)
  expect_string(po$id, label = label)
  expect_class(po$param_set, "ParamSet", label = label)
  expect_list(po$param_set$values, names = "unique", label = label)
  expect_flag(po$is_trained, label = label)
  expect_character(po$packages, any.missing = FALSE, unique = TRUE, label = label)
  expect_function(po$train, nargs = 1L)
  expect_function(po$predict, nargs = 1L)
  expect_function(po$.__enclos_env__$private$.train, nargs = 1L)
  expect_function(po$predict, nargs = 1L)
  expect_function(po$.__enclos_env__$private$.predict, nargs = 1L)
  expect_data_table(po$input, any.missing = FALSE)
  expect_names(names(po$input), permutation.of = c("name", "train", "predict"))
  expect_data_table(po$output, any.missing = FALSE)
  expect_names(names(po$output), permutation.of = c("name", "train", "predict"))
  expect_int(po$innum, lower = 1L)
  expect_int(po$outnum, lower = 1L)
  # at least one of "train" or "predict" must be in every parameter's tag
  expect_true(every(po$param_set$tags, function(x) {
    length(intersect(c("train", "predict"), x)) > 0
  }))
}
