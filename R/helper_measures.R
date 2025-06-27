# helper functions specific for survival measures

# Compute per-observation prediction errors for uncensored survival times.
#
# This function returns either squared or absolute differences between predicted
# event times and observed event times, for all uncensored observations.
# Useful for downstream aggregation (e.g., RMSE, MAE), or inspection of
# prediction residuals.
.obs_surv_errors = function(truth, response, method = "squared") {
  method = assert_choice(method, c("squared", "abs"))
  assert_surv(truth)

  is_event = truth[, 2L] == 1
  if (!any(is_event)) return(NA) # if no events, can't calculate score!
  event_times = truth[is_event, 1L]
  pred_times  = response[is_event]

  errors = switch(method,
    squared = (event_times - pred_times)^2,
    abs = abs(event_times - pred_times)
  )

  errors
}

.scoring_rule_erv = function(measure, prediction, task, train_set) {
  if (is.null(task) || is.null(train_set)) {
    stop("'task' and 'train_set' are required if 'ERV' is 'TRUE'")
  }

  measure$param_set$set_values(ERV = FALSE)
  # compute score for the learner
  learner_score = measure$score(prediction, task = task, train_set = train_set)

  # compute score for the baseline (Kaplan-Meier)
  # train KM
  km = lrn("surv.kaplan")$train(task = task, row_ids = train_set)
  # predict KM on the test set (= not train ids)
  test_set = setdiff(task$row_ids, train_set)
  km_pred = km$predict(task, row_ids = test_set)
  base_score = measure$score(km_pred, task = task, train_set = train_set)

  measure$param_set$set_values(ERV = TRUE)

  # return percentage decrease
  1 - (learner_score / base_score)
}

## constant interpolate CIF matrix to requested `new_times`
.interp_cif = function(cif_mat, new_times) {
  # predicted time points
  pred_times = as.numeric(colnames(cif_mat))
  if (all(new_times %in% pred_times)) {
    # no interpolation needed
    cif_mat[, as.character(new_times), drop = FALSE]
  } else {
    extend_times = getFromNamespace("C_Vec_WeightedDiscreteCdf", ns = "distr6")
    t(extend_times(new_times, pred_times, cdf = t(cif_mat), lower = TRUE, FALSE))
  }
}

## wrapper around `riskRegression::Score()`
.riskRegr_score = function(mat_list, metric, data, formula, times, cause) {
  assert_choice(metric, c("auc", "brier"))

  invoke(
    riskRegression::Score,
    mat_list, # list with one risk prediction matrix (n_obs x times)
    data = data, # (time, event) values for `formula` => n_rows == n_obs
    # `Hist(time, event) ~ 1 => cens.model = 'km') or `Hist(time, event) ~ vars` for 'cox'
    formula = formula,
    summary = base::switch(metric == "brier", "ibs"), # `NULL` otherwise
    se.fit = 0L,
    metrics = metric,
    cens.method = "ipcw",
    cens.model = "km", # "cox" if covariates in formula
    use.event.times = FALSE,
    null.model = FALSE,
    contrasts = FALSE,
    times = times,
    cause = cause
  )
}
