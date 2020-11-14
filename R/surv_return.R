#' @title Get Survival Predict Types
#' @description Internal helper function to easily return the correct survival predict types and to
#' automatically coerce a predicted survival probability matrix to a
#' [distr6::VectorDistribution] of [distr6::WeightedDiscrete] distributions.
#' @param times (`numeric()`) \cr Vector of survival times.
#' @param surv (`matrix()`)\cr Matrix of predicted survival probabilities, rows are observations,
#' columns are times. Number of columns should be equal to length of `times`.
#' @param crank (`numeric()`)\cr Relative risk/continuous ranking. Higher value is associated
#' with higher risk. If `NULL` then either set as `lp` if available or as the estimated
#' survival expectation, computed by `colSums(surv)`.
#' @param lp (`numeric()`)\cr Predicted linear predictor, used to impute `crank` if `NULL`.
#' @param response (`numeric()`)\cr Predicted survival time, passed through function without
#'modification.
#' @details
#' To avoid complications caused by degenerative distributions, if `0` is not in `times`, then
#' this is added as the first time-point and a column of `1`s is bound to the left of `surv`.
#' Additionally if the final column of `surv` is not all `1`s, then an additional time is added
#' as `max(times) + 1e-3` and a column of `1`s is bound to the right of `surv`.
#' @export
.surv_return = function(times = NULL, surv = NULL, crank = NULL, lp = NULL, response = NULL) {

  out = list()

  if ((!is.null(times) && is.null(surv)) || (is.null(times) && !is.null(surv))) {
    stop("Either both `times` and `surv` should be `NULL` or neither should be.")
  } else if (is.null(times) && is.null(surv)) {
    distr = NULL
  } else {
    # assume surv rows = observations and cols = times
    assert(length(times) == ncol(surv))

    # add predictions at time = 0 if not already present
    if (0 %nin% times) {
      times = c(0, times)
      surv = cbind(1, surv)
    }

    # add S(t) = 1 if not already present
    if (!all(surv[, ncol(surv)] == numeric(nrow(surv)))) {
      times = c(times, max(times) + 1e-3)
      surv = cbind(surv, 0)
    }

    cdf <- apply(surv, 1, function(.x) list(cdf = 1 - .x))
    out$distr <- distr6::VectorDistribution$new(
      distribution = "WeightedDiscrete",
      params = cdf,
      shared_params = list(x = as.numeric(times)),
      decorators = c("CoreStatistics", "ExoticStatistics")
    )
  }

  if (is.null(crank)) {
    if (!is.null(lp)) {
      # assumes PH-type lp where high value = high risk
      crank = lp
    } else if (is.null(times) | is.null(surv)) {
      stop("`times` and `surv` must be given if `crank` and `lp` are both NULL.")
    } else {
      # negative mean survival distribution
      crank = -apply(1 - surv, 1, function(.x) sum(c(.x[1], diff(.x)) * times))
    }
  }

  out$crank = crank
  out$lp = lp
  out$response = response

  out
}
