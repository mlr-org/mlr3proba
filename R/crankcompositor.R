#' @include PipeOpCrankCompositor.R
#'
#' @title Compose a Crank Predict Type for Survival Learners
#' @description This is a wrapper around the [mlr_pipeops_crankcompose] pipe operation, which
#' simplifies graph creation.
#' @param learner [LearnerSurv] object for which a `crank` is composed (or over-written)
#' @param method One of `mean` or `median`. Used to determine how `crank` is estimated from the
#' predicted `distr`.
#' @param param_vals Additional parameters to pass to the `learner`.
#' @details For full details see [mlr_pipeops_crankcompose].
#' @return [Graph]
#' @examples
#' library("mlr3")
#' library("mlr3pipelines")
#'
#' ranger.crank = crankcompositor(learner = lrn("surv.ranger"),
#'                             method = "median")
#' resample(tsk("rats"), ranger.crank, rsmp("cv", folds = 2))$predictions()
#' @export
crankcompositor = function(learner, method = "mean", param_vals = list()){
  pred = po("learner", learner, param_vals = param_vals)
  compositor = po("crankcompose", param_vals = list(method = method))

  pred %>>% compositor
}
