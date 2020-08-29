#' @title Visualization of fitted `LearnerSurv` objects
#' @description Wrapper around `predict.LearnerSurv` and `plot.VectorDistribution`.
#'
#' @importFrom graphics plot
#' @param x ([LearnerSurv])
#' @param task ([TaskSurv])
#' @param fun (`character`) \cr
#'   Passed to `distr6::plot.VectorDistribution`
#' @param row_ids (`integer()`) \cr
#'   Passed to `Learner$predict`
#' @param newdata (`data.frame()`) \cr
#'   If not missing `Learner$predict_newdata` is called instead of `Learner$predict`.
#' @param ... Additional arguments passed to `distr6::plot.VectorDistribution`
#'
#'
#' @examples
#' \dontrun{
#' library(mlr3)
#' task = tsk("rats")
#'
#' # Prediction Error Curves for prediction object
#' learn = lrn("surv.coxph")
#' learn$train(task)
#'
#' plot(learn, task, "survival", ind = 10)
#' plot(learn, task, "survival", row_ids = 1:5)
#' plot(learn, task, "survival", newdata = task$data()[1:5, ])
#' plot(learn, task, "survival", newdata = task$data()[1:5, ], ylim = c(0, 1))
#' }
#' @export
plot.LearnerSurv = function(
  x,
  task,
  fun = c("survival", "pdf", "cdf", "quantile", "hazard", "cumhazard"),
  row_ids = NULL,
  newdata,
  ...) {

  fun = match.arg(fun)

  if (missing(newdata)) {
    pred = x$predict(task = task, row_ids = row_ids)
  }
  else {
    pred = x$predict_newdata(newdata = newdata, task = task)
  }

  plot(pred$distr, fun = fun, ...)

}
