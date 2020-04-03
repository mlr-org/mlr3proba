#' @title Visualization of fitted `LearnerSurv` objects
#' @description Wrapper around `predict.LearnerSurv` and `plot.VectorDistribution`.
#'
#' @importFrom graphics plot
#' @param object ([LearnerSurv])
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
#' library(mlr3)
#' library(mlr3viz)
#' task = tsk("rats")
#'
#' # Prediction Error Curves for prediction object
#' learn = lrn("surv.coxph")
#' learn$train(task)
#'
#' autoplot(learn, task, "survival", ind = 10)
#' autoplot(learn, task, "survival", row_ids = 1:5)
#' autoplot(learn, task, "survival", newdata = task$data()[1:5,])
#' autoplot(learn, task, "survival", newdata = task$data()[1:5,], ylim=c(0, 1))
#'
#' ## problems
#' # autoplot(learn, task, "survival", ind = 1:5) # what's happening?
#' # autoplot(learn, task, "survival", n = 5) # confuses n with newdata
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
