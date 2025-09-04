# helper functions specific for survival measures

# S(t)/f(t) ESTIMATION/INTERPOLATION FUNCTIONS

#' @description
#' Linearly interpolate (and extrapolate) a survival curve at arbitrary time points.
#' @param surv_data `survfit` object or a `list` with 2 elemnts:
#' `surv` (survival probabilities) and corresponding `time` (time points)
#' @param eval_times vector of times (unordered, possibly duplicated)
#' @param method type of interpolation to use - `linear` (default) or `constant`
#' @return interpolated S(t) values
#' @noRd
.interp_surv = function(surv_data, eval_times, method = "linear") {
  assert_choice(method, c("linear", "constant"))

  # constant interpolation is easy
  if (method == "constant") {
    surv = surv_data$surv
    times = surv_data$time

    return(stats::approx(x = times, y = surv, xout = eval_times, yleft = 1,
                         method = "constant", rule = 2)$y)
  }

  # remove constant-interpolated values from S(t)
  unique_surv_idx = !duplicated(surv_data$surv)
  surv = surv_data$surv[unique_surv_idx] # decreasing
  times = surv_data$time[unique_surv_idx] # ordered

  # Edge case: constant survival
  if (all(surv == surv[1])) {
    return(rep(surv[1], length(eval_times)))
  }

  # linear interpolation (at least two S(t) values here)
  interp_surv = stats::approx(x = times, y = surv, xout = eval_times,
                              yleft = 1, method = "linear", rule = 2)$y

  # Extrapolate manually if needed
  min_time = min(times)
  max_time = max(times)

  # Precompute slopes for extrapolation
  slope_left = (surv[1L] - 1) / min_time
  slope_right = (surv[length(surv)] - surv[length(surv) - 1L]) / (max_time - times[length(times) - 1L])

  idx_left = eval_times < min_time
  idx_right = eval_times > max_time

  # Linear extrapolation considering that S(t = 0) = 1
  if (any(idx_left) && surv[1L] < 1) {
    interp_surv[idx_left] = 1 + slope_left * eval_times[idx_left]
  }

  # Linear extrapolation using the last time interval
  if (any(idx_right) && surv[length(surv)] > 0) {
    extrap_value = surv[length(surv)] + slope_right * (eval_times[idx_right] - max_time)
    interp_surv[idx_right] = pmax(0, extrap_value) # force S >= 0
  }

  interp_surv
}

#' @description PDF estimation from a survival curve
#' @param surv_data `survfit` object or a `list` with 2 elemnts:
#' `surv` (survival probabilities) and corresponding `time` (time points)
#' @param eval_times numeric vector (unordered, duplicated allowed)
#' @return numeric vector of density values f(t)
#' @noRd
.interp_pdf = function(surv_data, eval_times) {
  # keep all unique sorted times (predicted and requested) for pdf
  utimes = sort(unique(c(surv_data$time, eval_times)))

  # Create a mapping of `eval_times` to `utimes`
  indx = match(eval_times, utimes)

  # Linearly interpolate survival function (to avoid pdf = 0 problems)
  surv = .interp_surv(surv_data, utimes, method = "linear")

  # CDF = 1 - S
  cdf = 1 - surv

  # Numerical derivative: f = dF/dt = -dS/dt
  dt = diff(utimes)
  dF = diff(cdf)

  # Density (finite difference)
  dens = dF / dt

  # For timepoints exactly at utimes, align left
  dens_full = c(dens[1], dens) # replicate first slope for first point

  # return density at `eval_times`, clip any negatives to 0
  pmax(dens_full[indx], 0)
}

# Get predicted survival matrix from `PredictionSurv$data$distr` slot (if it exists)
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

# Wrapper function for Rcpp implementation of Gonen & Heller's concordance index.
# Takes a numeric vector of crank values (e.g., predicted scores) and a weight value
# for ties
.gonen = function(crank, tiex) {
  assert_numeric(crank, any.missing = FALSE)
  assert_number(tiex)

  c_gonen(sort(crank), tiex)
}

# Interfaces the Rcpp implementation to compute a weighted concordance index
# for right-censored survival data.
# Supports multiple weighting methods and handles tied predictions via `tiex`.
# Optionally uses Kaplan-Meier estimates from training data for weighting.
.cindex = function(truth, crank, t_max = NULL,
                   weight_meth = c("I", "G", "G2", "SG", "S"),
                   tiex = 0.5, train = NULL, eps = 1e-3) {
  if (length(unique(crank)) == 1L) {
    return(0.5)
  }

  assert_surv(truth)
  assert_numeric(crank)
  if (anyMissing(truth)) {
    return(NA_real_)
  }

  ord = order(truth[, "time"])
  time = as.double(truth[, "time"])[ord]
  status = as.integer(truth[, "status"])[ord]

  weight_meth = match.arg(weight_meth)

  if (weight_meth %in% c("I", "S")) {
    cens = matrix(ncol = 2L)
  } else {
    cens = survival::survfit(Surv(train[, "time"], 1 - train[, "status"]) ~ 1)
    cens = matrix(c(cens$time, cens$surv), ncol = 2L)
  }

  if (weight_meth == "SG" || weight_meth == "S") {
    surv = survival::survfit(train ~ 1)
    surv = matrix(c(surv$time, surv$surv), ncol = 2L)
  } else {
    surv = matrix(ncol = 2L)
  }

  if (is.null(t_max)) {
    t_max = max(time) + 1
  }

  cens[cens[, 2L] == 0, 2L] = eps
  surv[surv[, 2L] == 0, 2L] = eps

  c_concordance(time, status, crank[ord], t_max, weight_meth, cens, surv, tiex)
}

#' @description
#' Compute the Explained Residual Variation (ERV) of a survival prediction
#'
#' Calculates an R²-like statistic comparing the predictive performance of a learner
#' to a Kaplan–Meier baseline. The measure must support the `ERV` parameter flag.
#'
#' @param measure A `Measure` object (e.g., from `mlr3proba`) that supports the `ERV` parameter.
#' @param prediction A `PredictionSurv` object containing the learner's predictions on a test set.
#' @param task The `TaskSurv` object on which the learner and baseline are evaluated.
#' @param train_set Integer vector of row indices used for training the learner and baseline.
#'
#' @return A numeric value representing the explained residual variation:
#'   - `> 0` means the learner outperforms the Kaplan–Meier baseline.
#'   - `= 0` means the learner performs the same as the baseline.
#'   - `< 0` means the learner performs worse than the baseline.
#' @noRd
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
  # predict KM on the test set
  km_pred = km$predict(task, row_ids = prediction$row_ids)
  base_score = measure$score(km_pred, task = task, train_set = train_set)

  measure$param_set$set_values(ERV = TRUE)

  # return R^2-like score
  # 0 => same as base score, >0 => better than baseline
  # <0 => worse than baseline
  1 - (learner_score / base_score)
}

# Computes the integrated form of a time-dependent score (e.g., Brier score)
# via trapezoidal integration over the available time points.
.integrated_score = function(score, integrated) {
  # score is a matrix of BS(i,t) scores
  # rows => observations, cols => time points
  if (ncol(score) == 1L) {
    # Can't integrate over a single time point
    integrated = FALSE
  }

  if (!integrated) {
    # Return the mean score at each time point
    return(.col_sums(score))
  }

  times = as.numeric(colnames(score))
  n = length(times)
  time_diffs = diff(times)

  # Mean score at each time point (across observations)
  scores = .col_sums(score)

  # Apply trapezoidal rule and normalize by the time range
  sum(time_diffs * (scores[-n] + scores[-1]) / 2) / (max(times) - min(times))
}

# Computes column-wise means of a matrix while ignoring NA, NaN, and infinite values.
.col_sums = function(mat) {
  apply(mat, 2L, function(x) {
    x = x[is.finite(x)]
    mean(x, na.rm = TRUE)
  })
}
