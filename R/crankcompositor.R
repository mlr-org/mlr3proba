#' @include PipeOpCrankCompositor.R
#'
#' @title Compose a Crank Predict Type for Survival Learners
#' @description This is a wrapper around the [PipeOpCrankCompositor] pipe operation, which
#' simplifies graph creation.
#' @param learner [LearnerSurv] object for which a `crank` is composed (or over-written)
#' @param method One of `mean`, `mode`, or `median`; abbreviations allowed. Used to determine
#' how `crank` is estimated from the predicted `distr`. Default is `mean`.
#' @param param_vals Additional parameters to pass to the `learner`.
#' @details For full details see [PipeOpCrankCompositor].
#' @return [mlr3pipelines::GraphLearner]
#' @examples
#' library(mlr3)
#' library(mlr3pipelines)
#'
#' task = tgen("simsurv")$generate(20)
#' ranger.crank = crankcompositor(learner = lrn("surv.coxph"),
#'                             method = "median")
#' resample(task, ranger.crank, rsmp("cv", folds = 2))$predictions()
#' @export
crankcompositor = function(learner, method = c("mean","median","mode"), param_vals = list()){
  assert("distr" %in% learner$predict_types)

  pred = po("learner", learner, param_vals = param_vals)
  compositor = po("crankcompose", param_vals = list(method = match.arg(method)))

  GraphLearner$new(pred %>>% compositor)
}
