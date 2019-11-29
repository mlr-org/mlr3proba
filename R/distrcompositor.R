#' @include PipeOpDistrCompositor.R
#'
#' @title Compose a Distr Predict Type for Survival Learners
#' @description This is a wrapper around the [mlr_pipeops_distrcompose] pipe operation, which
#' simplifies graph creation.
#' @param learner [LearnerSurv] object for which a `distr` is composed (or over-written)
#' @param estimator One of `kaplan` or `nelson`, corresponding to the Kaplan-Meier and Nelson-Aalen
#' estimators respectively. Used to estimate the baseline survival distribution. Partial matching allowed.
#' @param form One of `aft`, `ph`, or `po`, corresponding to accelerated failure time, proportional hazards,
#' and proportional odds respectively. Used to determine the form of the composed survival distribution.
#' @param param_vals Additional parameters to pass to the `learner`.
#' @details For full details see [mlr_pipeops_distrcompose].
#' @return [Graph]
#' @examples
#' library("mlr3")
#' library("mlr3pipelines")
#'
#' cvglm.distr = distrcompositor(learner = lrn("surv.cvglmnet"),
#'                             estimator = "kaplan",
#'                             form = "aft")
#' resample(tsk("rats"), cvglm.distr, rsmp("cv", folds = 2))$predictions()
#' @export
distrcompositor = function(learner, estimator = "kaplan", form = "aft", param_vals = list()){
  pred = po("learner", learner, param_vals = param_vals)

  base = c("kaplan", "nelson")[charmatch(estimator, c("kaplan", "nelson"))]
  base = po("learner", lrn(paste("surv", base, sep = ".")))

  compositor = po("distrcompose", param_vals = list(form = form))

  gunion(list(base, pred)) %>>% compositor
}
