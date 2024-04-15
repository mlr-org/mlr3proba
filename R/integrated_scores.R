score_intslogloss = function(true_times, unique_times, cdf, eps = eps) {
  assert_number(eps, lower = 0)
  c_score_intslogloss(true_times, unique_times, cdf, eps = eps)
}

score_graf_schmid = function(true_times, unique_times, cdf, power = 2) {
  assert_number(power)
  c_score_graf_schmid(true_times, unique_times, cdf, power)
}


weighted_survival_score = function(loss, truth, distribution, times = NULL,
  t_max = NULL, p_max = NULL, proper, train = NULL, eps, ...) {
  assert_surv(truth)

  # if `tmax_apply` = TRUE, the t_max cutoff will be applied to both train
  # (if provided) and test data. For this at least one of `t_max` or `p_max`
  # should be given
  tmax_apply = !(is.null(t_max) && is.null(p_max))

  # calculate `t_max` (time horizon)
  if (is.null(times) || !length(times)) {
    unique_times = unique(sort(truth[, "time"]))
    if (!is.null(p_max)) {
      surv = survival::survfit(truth ~ 1)
      indx = which(1 - (surv$n.risk / surv$n) > p_max)
      if (length(indx) == 0) {
        # no indexes found, get last time point
        t_max = tail(surv$time, n = 1)
      } else {
        # first time point that surpasses the specified
        # `p_max` proportion of censoring
        t_max = surv$time[indx[1]]
      }
    } else if (is.null(t_max)) {
      t_max = max(unique_times)
    }
  } else {
    unique_times = .c_get_unique_times(truth[, "time"], times)
    t_max = max(unique_times)
  }

  # subset `unique_times` in the test set up to `t_max`
  unique_times = unique_times[unique_times <= t_max]

  # keep all the test set time points for the censoring distr via KM if no train data
  all_times  = truth[, "time"]
  all_status = truth[, "status"]

  # get the cdf matrix (rows => times, cols => obs)
  if (inherits(distribution, "Distribution")) {
    cdf = as.matrix(distribution$cdf(unique_times))
  }
  else if (inherits(distribution, "array")) {
    if (length(dim(distribution)) == 3) {
      # survival 3d array, extract median
      surv_mat = .ext_surv_mat(arr = distribution, which.curve = 0.5)
    } else { # survival 2d array
      surv_mat = distribution
    }
    surv_mat = surv_mat[, as.numeric(colnames(surv_mat)) <= t_max]
    mtc = findInterval(unique_times, as.numeric(colnames(surv_mat)))
    cdf = 1 - surv_mat[, mtc, drop = FALSE]
    if (any(mtc == 0)) {
      cdf = cbind(matrix(0, nrow(cdf), sum(mtc == 0)), cdf)
    }
    # apply `t_max` cutoff to remove observations in the test predictions
    cdf = cdf[all_times <= t_max, , drop = FALSE]
    colnames(cdf) = unique_times
    cdf = t(cdf)
  }

  # apply `t_max` cutoff to the test set's (time, status)
  true_times  = all_times [all_times <= t_max]
  true_status = all_status[all_times <= t_max]

  assert_numeric(true_times, any.missing = FALSE)
  assert_numeric(unique_times, any.missing = FALSE)
  assert_matrix(cdf, nrows = length(unique_times), ncols = length(true_times),
                any.missing = FALSE)

  # Note that whilst we calculate the score for censored here, they are then
  # corrected in the weighting function `.c_weight_survival_score()`
  if (loss == "graf") {
    score = score_graf_schmid(true_times, unique_times, cdf, power = 2)
  } else if (loss == "schmid") {
    score = score_graf_schmid(true_times, unique_times, cdf, power = 1)
  } else {
    score = score_intslogloss(true_times, unique_times, cdf, eps = eps)
  }

  # use all (time, status) information from train or test set
  if (is.null(train)) {
    cens = survival::survfit(Surv(all_times, 1 - all_status) ~ 1)
  } else {
    train_times  = train[, "time"]
    train_status = train[, "status"]
    cens = survival::survfit(Surv(train_times, 1 - train_status) ~ 1)
  }
  # G(t): KM estimate of the censoring distr
  cens = matrix(c(cens$time, cens$surv), ncol = 2)

  # filter time points based on `t_max` cutoff
  if (tmax_apply) {
    cens = cens[cens[,1] <= t_max, , drop = FALSE]
  }

  score = .c_weight_survival_score(score, truth, unique_times, cens, proper, eps)
  colnames(score) = unique_times

  return(score)
}

integrated_score = function(score, integrated, method = NULL) {
  # score is a matrix of BS(i,t) scores
  # rows => observations, cols => time points
  if (ncol(score) == 1) {
    integrated = FALSE
  }

  if (integrated) {
    # summary score (integrated across all time points)
    if (method == 1) {
      score = as.numeric(score)
      return(mean(score[is.finite(score)], na.rm = TRUE)) # remove NAs and Infs
    } else if (method == 2) {
      times = as.numeric(colnames(score))
      lt = ncol(score)
      score = col_sums(score) # score(t)
      return((diff(times) %*% (score[1:(lt - 1)] + score[2:lt])) / (2 * (max(times) - min(times))))
    }
  } else {
    return(col_sums(score)) # score(t)
  }
}

integrated_se = function(score, integrated) {
  if (integrated) {
    sqrt(sum(stats::cov(score), na.rm = TRUE) / (nrow(score) * ncol(score)^2))
  } else {
    apply(score, 2, function(x) stats::sd(x) / sqrt(nrow(score)))
  }
}

# like colMeans(), but removing Infs, NAs and NaNs
col_sums = function(mat) {
  apply(mat, 2, function(x) {
    x = x[is.finite(x)]
    mean(x, na.rm = TRUE)
  })
}
