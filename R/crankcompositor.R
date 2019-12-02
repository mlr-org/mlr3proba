#' @include PipeOpCrankCompositor.R
#'
#' @title Compose a Crank Predict Type for Survival Learners
#' @description This is a wrapper around the [mlr_pipeops_crankcompose] pipe operation, which
#' simplifies graph creation.
#' @param learner [LearnerSurv] object for which a `crank` is composed (or over-written)
#' @param method One of `mean` or `median`. Used to determine how `crank` is estimated from the
#' predicted `distr`.
#' @param param_vals Additional parameters to pass to the `learner`.
#' @details For full details see [PipeOpCrankCompositor].
#' @return [mlr3pipelines::GraphLearner]
#' @examples
#' library("mlr3")
#' library("mlr3pipelines")
#'
#' ranger.crank = crankcompositor(learner = lrn("surv.coxph"),
#'                             method = "median")
#' resample(tsk("rats"), ranger.crank, rsmp("cv", folds = 2))$predictions()
#' @export
crankcompositor = function(learner, method = "mean", param_vals = list()){
  assert("distr" %in% learner$predict_types)

  pred = po("learner", learner, param_vals = param_vals)
  compositor = po("crankcompose", param_vals = list(method = method))

  GraphLearner$new(pred %>>% compositor)
}
