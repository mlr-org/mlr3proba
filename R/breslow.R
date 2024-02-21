#' @title Survival probabilities using Breslow's estimator
#'
#' @description
#' Helper function to compose a survival distribution (or cumulative hazard)
#' from the relative risk predictions (linear predictors, `lp`) of a
#' **proportional hazards** model (e.g. a Cox-type model).
#'
#' @param times (`numeric()`)\cr Vector of times (train set).
#' @param status (`numeric()`)\cr Vector of status indicators (train set).
#' For each observation in the train set, this should be 0 (alive/censored) or
#' 1 (dead).
#' @param lp_train (`numeric()`)\cr Vector of linear predictors (train set).
#' These are the relative score predictions (\eqn{lp = \hat{\beta}X_{train}})
#' from a proportional hazards model on the train set.
#' @param lp_test (`numeric()`)\cr Vector of linear predictors (test set).
#' These are the relative score predictions (\eqn{lp = \hat{\beta}X_{test}})
#' from a proportional hazards model on the test set.
#' @param eval_times (`numeric()`)\cr Vector of times to compute survival
#' probabilities. If `NULL` (default), the unique and sorted `times` from the
#' train set will be used, otherwise the unique and sorted `eval_times`.
#' @param type (`character()`)\cr Type of prediction estimates.
#' Default is `surv` which returns the survival probabilities \eqn{S_i(t)} for
#' each test observation \eqn{i}. If `cumhaz`, the function returns the estimated
#' cumulative hazards \eqn{H_i(t)}.
#'
#' @details
#' We estimate the survival probability of individual \eqn{i} (from the test set),
#' at time point \eqn{t} as follows:
#' \deqn{S_i(t) = e^{-H_i(t)} = e^{-\hat{H}_0(t) \times e^{lp_i}}}
#'
#' where:
#' - \eqn{H_i(t)} is the cumulative hazard function for individual \eqn{i}
#' - \eqn{\hat{H}_0(t)} is Breslow's estimator for the **cumulative baseline
#' hazard**. Estimation requires the training set's `times` and `status` as well
#' the risk predictions (`lp_train`).
#' - \eqn{lp_i} is the risk prediction (linear predictor) of individual \eqn{i}
#' on the test set.
#'
#' Breslow's approach uses a non-parametric maximum likelihood estimation of the
#' cumulative baseline hazard function:
#'
#' \deqn{\hat{H}_0(t) = \sum_{i=1}^n{\frac{I(T_i \le t)\delta_i}
#' {\sum\nolimits_{j \in R_i}e^{lp_j}}}}
#'
#' where:
#' - \eqn{t} is the vector of time points (unique and sorted, from the train set)
#' - \eqn{n} is number of events (train set)
#' - \eqn{T} is the vector of event times (train set)
#' - \eqn{\delta} is the status indicator (1 = event or 0 = censored)
#' - \eqn{R_i} is the risk set (number of individuals at risk just before
#' event \eqn{i})
#' - \eqn{lp_j} is the risk prediction (linear predictor) of individual \eqn{j}
#' (who is part of the risk set \eqn{R_i}) on the train set.
#'
#' We employ **constant interpolation** to estimate the cumulative baseline hazards,
#' extending from the observed unique event times to the specified evaluation
#' times (`eval_times`).
#' Any values falling outside the range of the estimated times are assigned as
#' follows:
#' \deqn{\hat{H}_0(eval\_times < min(t)) = 0} and
#' \deqn{\hat{H}_0(eval\_times > max(t)) = \hat{H}_0(max(t))}
#'
#' Note that in the rare event of `lp` predictions being `Inf` or `-Inf`, the
#' resulting cumulative hazard values become `NaN`, which we substitute with
#' `Inf` (and corresponding survival probabilities take the value of \eqn{0}).
#'
#' For similar implementations, see `gbm::basehaz.gbm()`, `C060::basesurv()` and
#' `xgboost.surv::sgb_bhaz()`.
#'
#' @return a `matrix` (obs x times). Number of columns is equal to `eval_times`
#' and number of rows is equal to the number of test observations (i.e. the
#' length of the `lp_test` vector). Depending on the `type` argument, the matrix
#' can have either survival probabilities (0-1) or cumulative hazard estimates
#' (0-`Inf`).
#'
#' @references
#' `r format_bib("cox_1972", "lin_2007")`
#'
#' @examples
#' task = tsk("rats")
#' part = partition(task, ratio = 0.8)
#'
#' learner = lrn("surv.coxph")
#' learner$train(task, part$train)
#' p_train = learner$predict(task, part$train)
#' p_test  = learner$predict(task, part$test)
#'
#' surv = breslow(times = task$times(part$train), status = task$status(part$train),
#'                lp_train = p_train$lp, lp_test = p_test$lp)
#' head(surv)
#' @export
breslow = function(times, status, lp_train, lp_test, eval_times = NULL, type = "surv") {
  assert_numeric(times, null.ok = FALSE)
  assert_numeric(status, null.ok = FALSE)
  assert_numeric(lp_train, null.ok = FALSE)
  assert_true(length(times) == length(status))
  assert_true(length(times) == length(lp_train))
  assert_numeric(lp_test, null.ok = FALSE)
  assert_numeric(eval_times, null.ok = TRUE)
  assert_subset(type, choices = c("surv", "cumhaz"), empty.ok = FALSE)

  # cumulative baseline hazard
  cbhaz = .cbhaz_breslow(times = times, status = status, lp = lp_train,
                         eval_times = eval_times)

  cumhaz = exp(lp_test) %*% t(cbhaz)

  # (Inf * 0) or (0 * Inf) cases
  # Inf predicted risk or Inf cumulative baseline hazard
  cumhaz[is.nan(cumhaz)] = Inf

  if (type == "surv") {
    return(exp(-cumhaz))
  } else {
    return(cumhaz)
  }
}

# Breslow's Cumulative Baseline Hazard helper function
#
# Computes the cumulative baseline hazard using Breslow's (1972) estimator.
# Assumes risk predictions are derived from a **proportional hazards**
# regression model.
#
# @param times (`numeric()`)\cr Vector of times (train set).
# @param status (`numeric()`)\cr Vector of status indicators (train set).
# For each observation in the train set, this should be 0 (alive/censored) or
# 1 (dead).
# @param lp (`numeric()`)\cr Vector of linear predictors (train set).
# These are the relative score predictions from a proportional hazards model
# on the train set.
# @param eval_times (`numeric()`)\cr Vector of times to compute the cumulative
# baseline hazard estimations. If `NULL` (default), the unique and sorted
# `times` from the train set will be used, otherwise the unique and sorted
# `eval_times`.
#
# @returns
# A vector of cumulative baseline hazards, one per (increasing) time
# point. The times are added as names in the output vector.
.cbhaz_breslow = function(times, status, lp, eval_times = NULL) {
  # unique, sorted event times
  event_times = sort(unique(times[status == 1]))

  # baseline (non-cumulative) hazards are first evaluated on the specific `event_times`
  bhaz = vapply(event_times, function(et) {
    sum(times[status == 1] == et) / sum(exp(lp[times >= et]))
  }, numeric(1))

  # `eval_times` will be the sorted unique times (not just events)
  eval_times = sort(unique(eval_times %||% times))
  if (length(event_times) == 0) {
    # 0 events (training data has only censored observations!)
    res = numeric(length(eval_times))
  } else {
    # constant interpolation of cumulative hazards across `eval_times`
    # rule = 1:2 means return NAs for `xout < x` and max(y) for `xout > x`
    # NAs are overwritten with zeros (`yleft = 0`)
    res = stats::approx(x = event_times, y = cumsum(bhaz), yleft = 0,
      method = "constant", xout = eval_times, rule = 1:2)$y
  }

  names(res) = eval_times
  res
}
