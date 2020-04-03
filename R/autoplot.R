#' @title Visualization of fitted `LearnerSurv` objects
#' @description Wrapper around `predict.LearnerSurv` and `plot.VectorDistribution`.
#'
#' @inherit mlr3viz::autoplot
#' @param object An object of class `LearnerSurv`
#' @param task An object of class `TaskSurv`
#' @param fun
#'
#'
#' @examples
#' library(mlr3)
#' task = tsk("rats")
#'
#' # Prediction Error Curves for prediction object
#' learn = lrn("surv.coxph")
#' learn$train(task)
#'
#' autoplot(learn, task, "survival", ind = 10)
#' autoplot(learn, task, "survival", row_ids = 1:10)
#' autoplot(learn, task, "survival", newdata = task$data()[1:10,])
#' autoplot(learn, task, "survival", newdata = task$data()[1:10,], ylim=c(0, 1))
#'
#' ## problems
#' autoplot(learn, task, "survival", ind = 1:5) # what's happening?
#' autoplot(learn, task, "survival", n = 5) # confuses n with newdata
#'
#'
#' @export
autoplot.LearnerSurv = function(
  object,
  task,
  fun     = c("pdf","cdf","quantile", "survival", "hazard", "cumhazard"),
  row_ids = NULL,
  newdata,
  ...) {

  fun = match.arg(fun)

  if(missing(newdata)) {
    pred = object$predict(task = task, row_ids = row_ids)
  }
  else {
    pred = object$predict_newdata(newdata = newdata, task = task)
  }

  plot(pred$distr, fun = fun, ...)

}
