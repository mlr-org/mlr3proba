#' @title Calibration Plot for `PredictionSurv` Objects
#' @description Compares the average predicted survival distribution to a Kaplan-Meier estimate.
#'
#' @importFrom graphics plot lines legend
#' @param x ([PredictionSurv])
#' @param task ([TaskSurv]) \cr
#'   For Kaplan-Meier learner, passed to `$predict`.
#' @param row_ids (`integer()`) \cr
#'   For Kaplan-Meier learner, passed to `$predict`.
#' @param times (`numeric()`) \cr
#'   Times for plotting over, if `NULL` uses all times from `task`.
#' @param ... Additional arguments, currently unused.
#'
#' @examples
#' \donttest{
#' learn = lrn("surv.coxph")
#' task = tsk("rats")
#' p = learn$train(task, row_ids = 1:100)$predict(task, row_ids = 101:200)
#' plot(p, task)
#' }
#'
#' @export
plot.PredictionSurv = function(x, task, row_ids = NULL, times = NULL, ...) {

  assert("distr" %in% x$predict_types)
  pred_distr = distr6::as.MixtureDistribution(x$distr)

  km = lrn("surv.kaplan")
  km_pred = km$train(task, row_ids = row_ids)$predict(task, row_ids = row_ids)
  km_distr = distr6::as.MixtureDistribution(km_pred$distr)

  if (is.null(times)) {
    times = sort(unique(task$truth()[, 1]))
  }

  plot(x = times, y = 1 - km_distr$cdf(times), type = "l", xlab = "T", ylab = "S(T)")
  lines(x = times, y = 1 - pred_distr$cdf(times), type = "l", col = 2)
  legend("topright", col = 1:2, lwd = 1, legend = c("Kaplan-Meier", "Prediction"))
}
