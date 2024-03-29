score_intslogloss = function(true_times, unique_times, cdf, eps = eps) {
  assert_number(eps, lower = 0)
  c_score_intslogloss(true_times, unique_times, cdf, eps = eps)
}

score_graf_schmid = function(true_times, unique_times, cdf, power = 2) {
  assert_number(power)
  c_score_graf_schmid(true_times, unique_times, cdf, power)
}


weighted_survival_score = function(loss, truth, distribution, times, t_max, p_max, proper, train = NULL, eps, ...) {

  assert_surv(truth)

  if (is.null(times) || !length(times)) {
    unique_times = unique(sort(truth[, "time"]))
    if (!is.null(t_max)) {
      unique_times  = unique_times[unique_times <= t_max]
    } else if (!is.null(p_max)) {
      s = survival::survfit(truth ~ 1)
      t_max = s$time[which(1 - s$n.risk / s$n > p_max)[1]]
      unique_times = unique_times[unique_times <= t_max]
    }
  } else {
    unique_times = .c_get_unique_times(truth[, "time"], times)
  }

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
    mtc = findInterval(unique_times, as.numeric(colnames(surv_mat)))
    cdf = 1 - t(surv_mat[, mtc])
    if (any(mtc == 0)) {
      cdf = rbind(matrix(0, sum(mtc == 0), ncol(cdf)), cdf)
    }
    rownames(cdf) = unique_times
  }

  true_times = truth[, "time"]

  assert_numeric(true_times, any.missing = FALSE)
  assert_numeric(unique_times, any.missing = FALSE)
  assert_matrix(cdf, nrows = length(unique_times), ncols = length(true_times), any.missing = FALSE)

  ## Note that whilst we calculate the score for censored here, they are then
  ##  corrected in the weighting function
  if (loss == "graf") {
    score = score_graf_schmid(true_times, unique_times, cdf, power = 2)
  } else if (loss == "schmid") {
    score = score_graf_schmid(true_times, unique_times, cdf, power = 1)
  } else {
    score = score_intslogloss(true_times, unique_times, cdf, eps = eps)
  }

  if (is.null(train)) {
    cens = survival::survfit(Surv(truth[, "time"], 1 - truth[, "status"]) ~ 1)
  } else {
    cens = survival::survfit(Surv(train[, "time"], 1 - train[, "status"]) ~ 1)
  }

  score = .c_weight_survival_score(score, truth, unique_times, matrix(c(cens$time, cens$surv), ncol = 2), proper, eps)
  colnames(score) = unique_times

  return(score)
}

integrated_score = function(score, integrated, method = NULL) {
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
