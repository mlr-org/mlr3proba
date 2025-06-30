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
## - Only one of `times`, `t_max`, `p_max` should be non-NULL.
## - `times`: sorted, unique, positive.
## - `t_max` > 0; `p_max` in [0,1]
.weighted_survival_score = function(loss = "graf", truth, distribution, times = NULL,
  t_max = NULL, p_max = NULL, train = NULL, eps = 1e-3) {
  # input checks
  assert_choice(loss, choices = c("graf", "schmid", "logloss"), null.ok = FALSE)
  assert_surv(truth)
  assert_numeric(times, lower = 0, sorted = TRUE, unique = TRUE, null.ok = TRUE,
                 any.missing = FALSE, min.len = 1)
  assert_number(t_max, lower = 0, null.ok = TRUE)
  assert_number(p_max, lower = 0, upper = 1, null.ok = TRUE)
  assert_surv(train, null.ok = TRUE)
  assert_number(eps, lower = 0)

  # test set's (times, status)
  test_times = truth[, 1L]
  test_status = truth[, 2L]

  # Determine time horizon if applicable
  tmax_apply = !(is.null(t_max) && is.null(p_max))

  # **IMPORTANT**: times to calculate the score at => evaluation times
  # We start with the unique, sorted, test set time points
  eval_times = unique(sort(test_times))

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
    if (t_max < min(eval_times)) {
      stop("`t_max` is smaller than the minimum test time. Please increase value!")
    }

    # filter `eval_times` in the test set up to `t_max`
    eval_times = eval_times[eval_times <= t_max]
  } else {
    # `times` can be provided or it is NULL
    # If some requested times are outside the evaluation range, warn the user
    if (!is.null(times)) {
      outside_range = any(times < min(eval_times) | times > max(eval_times))
      if (outside_range) {
        warning("Some requested times are outside the evaluation range (sorted unique test times).")
      }
    }

    # Use requested times if given, else default to `eval_times`
    eval_times = times %??% eval_times
  }

  # Get the CDF matrix [times x observations]
  if (inherits(distribution, "Distribution")) {
    cdf = as.matrix(distribution$cdf(eval_times))
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
    # `eval_times`: time points for which we want S(t)
    cdf = extend_times(eval_times, pred_times, cdf = t(1 - surv_mat), TRUE, FALSE)
    rownames(cdf) = eval_times
  }

  # check: CDF matrix is [times x test obs]
  assert_matrix(cdf, nrows = length(eval_times), ncols = length(test_times), any.missing = FALSE)

  # Compute score matrix [obs x times], by using:
  # - S^2 or (1-S)^2 for Graf score (ISBS)
  # - S or (1-S) for Schmid score (ISS)
  # - log(S) or log(1-S) for integrated LogLoss score (ISLL)
  score = switch(loss,
    logloss = c_score_logloss(test_times, eval_times, cdf, eps = eps),
    schmid  = c_score_graf_schmid(test_times, eval_times, cdf, power = 1),
    graf    = c_score_graf_schmid(test_times, eval_times, cdf, power = 2)
  )

  # Compute G(t): KM estimate of the censoring distribution from train or test set
  cens_source = if (is.null(train)) truth else train
  cens_fit = survival::survfit(Surv(cens_source[, "time"], 1 - cens_source[, "status"]) ~ 1)
  cens = matrix(c(cens_fit$time, cens_fit$surv), ncol = 2L)

  ipcw_score = c_apply_ipcw_weights(score, truth, eval_times, cens, eps)
  colnames(ipcw_score) = eval_times

  ipcw_score
}
