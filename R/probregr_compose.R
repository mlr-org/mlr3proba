#' @title Create a Distr Predict Type for Regression Learners
#' @description This is a wrapper around the [PipeOpProbregrCompositor] pipe operation, which
#' simplifies graph creation.
#' @param learner [LearnerSurv] object for which a `distr` is composed.
#' @param dist Location-scale distribution to use for composition. Current possibilities are
#' `"Cauchy", "Gumbel", "Laplace", "Logistic", "Normal` (default).
#' @param param_vals Additional parameters to pass to the `learner`.
#' @details For full details see [PipeOpProbregrCompositor].
#' @return [mlr3pipelines::GraphLearner]
#' @examples
#' \dontrun{
#' library(mlr3)
#' library(mlr3pipelines)
#'
#' task = tsk("boston_housing")
#' feat_distr = probregr_compose(
#'   learner = lrn("regr.featureless", predict_type = "se"),
#'   dist = "Logistic")
#' resample(task, feat_distr, rsmp("cv", folds = 2))$predictions()
#' }
#' @export
probregr_compose = function(learner, dist = "Normal", param_vals = list()) {

  pred = mlr3pipelines::po("learner", learner, param_vals = param_vals)
  compositor = mlr3pipelines::po("probregr_compose", param_vals = list(dist = dist))

  mlr3pipelines::GraphLearner$new(mlr3pipelines::`%>>%`(pred, compositor))
}
