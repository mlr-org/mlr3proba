weighted_survival_score = function(loss, truth, distribution, times, proper, train = NULL,
  eps, ...) {
  assert_surv(truth)
  # assertDistribution(distribution)

  if (is.null(times) || !length(times)) {
    unique_times = unique(sort(truth[, "time"]))
  } else {
    unique_times = .c_get_unique_times(truth[, "time"], times)
  }

  if (inherits(distribution, "Distribution")) {
    cdf = as.matrix(distribution$cdf(unique_times))
  } else {
    cdf = t(as.matrix(1 - distribution[, as.character(unique_times)]))
  }

  true_times <- truth[, "time"]

  ## Note that whilst we calculate the score for censored here, they are then
  ##  corrected in the weighting function
  if (loss == "graf") {
    score = c_score_graf_schmid(true_times, unique_times, cdf, power = 2)
  } else if (loss == "schmid") {
    score = c_score_graf_schmid(true_times, unique_times, cdf, power = 1)
  } else {
    score = c_score_intslogloss(true_times, unique_times, cdf, eps = eps)
  }

  if (is.null(train)) {
    cens = survival::survfit(Surv(truth[, "time"], 1 - truth[, "status"]) ~ 1)
  } else {
    cens = survival::survfit(Surv(train[, "time"], 1 - train[, "status"]) ~ 1)
  }

  score = .c_weight_survival_score(score, truth, unique_times,
    matrix(c(cens$time, cens$surv), ncol = 2),
    proper, eps)
  colnames(score) = unique_times

  return(score)
}

integrated_score = function(score, integrated, method = NULL) {
  if (ncol(score) == 1) {
    integrated = FALSE
  }

  if (integrated) {
    if (method == 1) {
      return(mean(as.numeric(score), na.rm = TRUE))
    } else if (method == 2) {
      times = as.numeric(colnames(score))
      lt = ncol(score)
      score = as.numeric(colMeans(score, na.rm = TRUE))
      return((diff(times) %*% (score[1:(lt - 1)] + score[2:lt])) / (2 * (max(times) - min(times))))
    }
  } else {
    return(colMeans(score, na.rm = TRUE))
  }
}

integrated_se = function(score, integrated) {
  if (integrated) {
    return(sqrt(sum(stats::cov(score)) / (nrow(score) * ncol(score)^2)))
  } else {
    return(apply(score, 2, function(x) stats::sd(x) / sqrt(nrow(score))))
  }
}
