# helper functions specific for survival measures

# SURVIVAL ----
# S(t)/f(t) ESTIMATION/INTERPOLATION FUNCTIONS

# Linearly interpolate (and extrapolate) a survival curve
# @param fit `survfit` object or a `list` with 2 elemnts:
# `surv` (survival probabilities) and corresponding `time` (time points)
# @param new_times vector of times (unordered, possibly duplicated)
# @param inter_type type of interpolation to use - `linear` (default) or `constant`
# @return interpolated S(t) values
.interp_surv = function(fit, new_times, inter_type = "linear") {
  assert_choice(inter_type, c("linear", "constant"))

  # constant interpolation is easy
  if (inter_type == "constant") {
    surv = fit$surv
    times = fit$time

    return(stats::approx(x = times, y = surv, xout = new_times, yleft = 1,
                         method = "constant", rule = 2)$y)
  }

  # remove constant-interpolated values from S(t)
  keep = !duplicated(fit$surv)
  surv = fit$surv[keep] # decreasing
  times = fit$time[keep] # ordered

  # Edge case: constant survival
  if (length(unique(surv)) == 1) {
    return(rep(surv[1], length(new_times)))
  }

  # linear interpolation (at least two S(t) values here)
  int_surv = stats::approx(x = times, y = surv, xout = new_times,
                           yleft = 1, method = "linear", rule = 2)$y

  # Extrapolate manually if needed
  min_time = min(times)
  max_time = max(times)

  # Precompute slopes for extrapolation
  slope_min = (surv[1L] - 1) / min_time
  slope_max = (surv[length(surv)] - surv[length(surv) - 1L]) / (max_time - times[length(times) - 1L])

  idx_min = new_times < min_time
  idx_max = new_times > max_time

  # Linear extrapolation considering that S(t = 0) = 1
  if (any(idx_min) && surv[1L] < 1) {
    int_surv[idx_min] = 1 + slope_min * new_times[idx_min]
  }

  # Linear extrapolation using the last time interval
  if (any(idx_max) && surv[length(surv)] > 0) {
    extrapolated_value = surv[length(surv)] + slope_max * (new_times[idx_max] - max_time)
    int_surv[idx_max] = pmax(0, extrapolated_value) # force S >= 0
  }

  int_surv
}

# PDF estimation from a survival curve
# @param fit `survfit` object or a `list` with 2 elemnts:
# `surv` (survival probabilities) and corresponding `time` (time points)
# @param new_times numeric vector (unordered, duplicated allowed)
# @return numeric vector of density values f(t)
.interp_pdf = function(fit, new_times) {
  # keep all unique sorted times (predicted and requested) for pdf
  utimes = sort(unique(c(fit$time, new_times)))

  # Create a mapping of `new_times` to `utimes`
  indx = match(new_times, utimes)

  # Linearly interpolate survival function (to avoid pdf = 0 problems)
  surv = .interp_surv(fit, utimes, inter_type = "linear")

  # CDF = 1 - S
  cdf = 1 - surv

  # Numerical derivative: f = dF/dt
  dt = diff(utimes)
  dF = diff(cdf)

  # Density (finite difference)
  dens = dF / dt

  # For timepoints exactly at utimes, align left
  dens_full = c(dens[1], dens) # replicate first slope for first point

  # return density at `new_times`, clip any negatives to 0
  pmax(dens_full[indx], 0)
}

# get predicted survival matrix from `PredictionSurv$data$distr` slot (if it exists)
.get_surv_matrix = function(prediction) {
  if (inherits(prediction$data$distr, "array")) {
    surv = prediction$data$distr
    if (length(dim(surv)) == 3L) {
      # survival 3d array, extract median
      surv = .ext_surv_mat(arr = surv, which.curve = 0.5)
    }
  } else {
    stop("Distribution prediction does not have a survival matrix or array in the `$data$distr` slot")
  }

  surv
}

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

  # return R^2-like score
  # 0 => same as base score, >0 => better than baseline
  # <0 => worse than baseline
  1 - (learner_score / base_score)
}

# COMPETING RISKS ----
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
