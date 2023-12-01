#' @title Survival probabilities using Breslow's estimator
#'
#' @description
#' Helper function to compose a survival distribution from the relative risk
#' predictions (`lp`) of a **proportional hazards** model (e.g. a Cox-type model).
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
#'
#' @details
#' We estimate the survival probability of individual \eqn{i} (from the test set),
#' at time point \eqn{t} as follows:
#' \deqn{S_i(t) = e^{-H_i(t)} = e^{-\hat{H}_0(t) \times e^{lp_i}}}
#'
#' where:
#' - \eqn{H_i(t)} is the cumulative hazard function for individual \eqn{i}
#' - \eqn{\hat{H}_0(t)} is Breslow's estimator for the cumulative baseline
#' hazard. Estimation requires the training set's `times` and `status` as well
#' the risk predictions (`lp_train`), see more details in [cbhaz_breslow])
#' - \eqn{lp_i} is the risk prediction (linear predictor) of individual \eqn{i}
#' on the test set.
#'
#' @return a survival probability `matrix` (obs x times). Number of columns is
#' equal to `eval_times` and number of rows is equal to the number of test
#' observations (i.e. the length of the `lp_test` vector).
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
#' surv = surv_breslow(times = task$times(part$train), status = task$status(part$train),
#'                     lp_train = p_train$lp, lp_test = p_test$lp)
#' head(surv)
#' @export
surv_breslow = function(times, status, lp_train, lp_test, eval_times = NULL) {
  assert_numeric(times, null.ok = FALSE)
  assert_numeric(status, null.ok = FALSE)
  assert_numeric(lp_train, null.ok = FALSE)
  assert_numeric(lp_test, null.ok = FALSE)
  assert_numeric(eval_times, null.ok = TRUE)

  base_haz = .cbhaz_breslow(times = times, status = status, lp = lp_train,
                            eval_times = eval_times)
  surv = exp(exp(lp_test) %*% -t(base_haz))
  rownames(surv) = 1:nrow(surv)
  surv
}

#' @title Breslow's Cumulative Baseline Hazard
#'
#' @description
#' Computes the cumulative baseline hazard using Breslow's (1972) estimator.
#' Assumes risk predictions are derived from a **proportional hazards**
#' regression model.
#'
#' @param times (`numeric()`)\cr Vector of times (train set).
#' @param status (`numeric()`)\cr Vector of status indicators (train set).
#' For each observation in the train set, this should be 0 (alive/censored) or
#' 1 (dead).
#' @param lp (`numeric()`)\cr Vector of linear predictors (train set).
#' These are the relative score predictions from a proportional hazards model
#' on the train set.
#' @param eval_times (`numeric()`)\cr Vector of times to compute the cumulative
#' baseline hazard estimations. If `NULL` (default), the unique and sorted
#' `times` from the train set will be used, otherwise the unique and sorted
#' `eval_times`.
#'
#' @details
#' Breslow's approach uses a non-parametric maximum likelihood estimation of the
#' cumulative baseline hazard function:
#'
#' \deqn{\hat{H}_0(t) = \sum_{i=1}^n{\frac{I(T_i \le t)\delta_i}
#' {\sum\nolimits_{j \in R_i}e^{lp_j}}}}
#'
#' where:
#' - \eqn{\hat{H}_0(t)} is the cumulative baseline hazard
#' - \eqn{t} is the vector of time points
#' - \eqn{n} is number of events
#' - \eqn{T} is the vector of event times
#' - \eqn{\delta} is the status indicator (event or censoring)
#' - \eqn{R_i} is the risk set (number of individuals at risk just before
#' event \eqn{i})
#' - \eqn{lp_j} is the risk prediction (linear predictor) of individual \eqn{j}
#' (who is part of the risk set \eqn{R_i}).
#'
#' For similar implementations, see `gbm::basehaz.gbm()`, `C060::basesurv()` and
#' `xgboost.surv::sgb_bhaz()`.
#'
#' @returns
#' A vector of cumulative baseline hazards, one per (increasing) time
#' point. The times are added as names in the output vector.
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
#'
#' base_haz = .cbhaz_breslow(times = task$times(part$train),
#'   status = task$status(part$train), lp = p_train$lp)
#' head(base_haz)
#'
#'@export
.cbhaz_breslow = function(times, status, lp, eval_times) {
  utimes = sort(unique(times[status == 1])) # unique, sorted event times
  bhaz = vapply(utimes, function(ut) sum(times[status == 1] == ut) / sum(exp(lp[times >= ut])), numeric(1))

  # constant interpolation of cumulative hazards across `eval_times`
  eval_times = sort(unique(eval_times %||% times))
  res = stats::approx(x = utimes, y = cumsum(bhaz), yleft = 0,
                      xout = eval_times, rule = 2)$y
  names(res) = eval_times
  res
}
