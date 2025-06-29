## Wrapper function for evaluating (possibly integrated) time-dependent survival scores.
## Interfaces with Rcpp scoring functions (e.g., Schmid, Graf, Integrated LogLoss Score).
##
## Selects appropriate evaluation times based on:
## - Explicit `times` argument,
## - Censoring proportion cutoff `p_max`, or
## - Time horizon `t_max`.
##
## Applies IPCW using training or test censoring distribution.
##
## Returns: matrix of scores with columns => evaluation times.
## Notes:
## - Either all of `times`, `t_max`, `p_max` are NULL, or only one of them is not
## - `times` is sorted (increasing), unique, positive time points
## - `t_max` > 0
## - `p_max` in [0,1]
.weighted_survival_score = function(loss, truth, distribution, times = NULL,
  t_max = NULL, p_max = NULL, proper, train = NULL, eps, remove_obs = FALSE) {
  assert_surv(truth)
  # test set's (times, status)
  test_times = truth[, "time"]
  test_status = truth[, "status"]

  # - `tmax_apply` = TRUE => one of `t_max`, `p_max` is given
  # - `tmax_apply` = FALSE => `times` is given or all of `times`, `p_max` and `t_max` are NULL
  # The `t_max` cutoff will be applied later in the predicted survival matrix
  # to filter observations (rows) and time points (columns) + filter the
  # (time, status) target on both train (if provided) and test data
  tmax_apply = !(is.null(t_max) && is.null(p_max))

  # **IMPORTANT**: times to calculate the score at => evaluation times
  # We start with the unique, sorted, test set time points
  unique_times = unique(sort(test_times))

  if (tmax_apply) {
    # one of `t_max`, `p_max` is given
    # calculate `t_max` (time horizon) if `p_max` is given
    if (!is.null(p_max)) {
      surv = survival::survfit(truth ~ 1)
      censored_proportion = 1 - (surv$n.risk / surv$n)
      indx = which(censored_proportion > p_max)

      t_max = if (length(indx) == 0L) {
         tail(surv$time, n = 1L) # no indexes found, use last time point
      } else {
        surv$time[indx[1L]]  # first time exceeding `p_max` censoring
      }
    }

    # check that `t_max` is within evaluation time range
    if (t_max < min(unique_times)) {
      stop("`t_max` is smaller than the minimum test time. Please increase value!")
    }

    # filter `unique_times` in the test set up to `t_max`
    unique_times = unique_times[unique_times <= t_max]
  } else {
    # `times` can be provided or it is NULL
    # If some requested times are outside the evaluation range, warn the user
    # We assume that `times` are positive, unique and sorted
    if (!is.null(times)) {
      outside_range = any(times < min(unique_times) | times > max(unique_times))
      if (outside_range) {
        warning("Some requested times are outside the evaluation range (unique, sorted test times).")
      }
    }

    # Use requested times if given, else default to `unique_times`
    unique_times = times %??% unique_times
  }

  # get the cdf matrix (rows => times, cols => obs)
  if (inherits(distribution, "Distribution")) {
    cdf = as.matrix(distribution$cdf(unique_times))
  }
  else if (inherits(distribution, "array")) {
    if (length(dim(distribution)) == 3L) {
      # survival 3d array, extract median
      surv_mat = .ext_surv_mat(arr = distribution, which.curve = 0.5)
    } else { # survival 2d array
      surv_mat = distribution
    }

    # `pred_times`: time points for which we have S(t)
    pred_times = as.numeric(colnames(surv_mat))
    extend_times = getFromNamespace("C_Vec_WeightedDiscreteCdf", ns = "distr6")
    # `unique_times`: time points for which we want S(t)
    cdf = extend_times(unique_times, pred_times, cdf = t(1 - surv_mat), TRUE, FALSE)
    rownames(cdf) = unique_times # times x obs
  }

  # apply `t_max` cutoff to remove observations as a preprocessing step to alleviate inflation
  if (tmax_apply && remove_obs) {
    true_times = test_times[test_times <= t_max]
    true_status = test_status[test_times <= t_max]
    cdf = cdf[, test_times <= t_max, drop = FALSE]
  } else {
    true_times = test_times
    true_status = test_status
  }
  true_truth = Surv(true_times, true_status)

  assert_numeric(true_times, any.missing = FALSE)
  assert_numeric(unique_times, any.missing = FALSE)
  assert_matrix(cdf, nrows = length(unique_times), ncols = length(true_times),
                any.missing = FALSE)

  # Note that whilst we calculate the score for censored observations here,
  # they are then corrected in the weighting function `c_weight_survival_score()`
  if (loss == "graf") {
    score = .score_graf_schmid(true_times, unique_times, cdf, power = 2)
  } else if (loss == "schmid") {
    score = .score_graf_schmid(true_times, unique_times, cdf, power = 1)
  } else {
    score = .score_intslogloss(true_times, unique_times, cdf, eps = eps)
  }

  # use the `truth` (time, status) information from the train or test set
  if (is.null(train)) {
    # no filtering of observations from test data: use ALL
    cens = survival::survfit(Surv(test_times, 1 - test_status) ~ 1)
  } else {
    # no filtering of observations from train data: use ALL
    train_times = train[, "time"]
    train_status = train[, "status"]
    cens = survival::survfit(Surv(train_times, 1 - train_status) ~ 1)
  }
  # G(t): KM estimate of the censoring distribution
  cens = matrix(c(cens$time, cens$surv), ncol = 2L)

  score = c_weight_survival_score(score, true_truth, unique_times, cens, proper, eps)
  colnames(score) = unique_times

  score
}

.score_intslogloss = function(true_times, unique_times, cdf, eps = eps) {
  assert_number(eps, lower = 0)
  c_score_intslogloss(true_times, unique_times, cdf, eps = eps)
}

.score_graf_schmid = function(true_times, unique_times, cdf, power = 2) {
  assert_number(power)
  c_score_graf_schmid(true_times, unique_times, cdf, power)
}
