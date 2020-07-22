#' @include PipeOpDistrCompositor.R
#'
#' @title Compose a Distr Predict Type for Survival Learners
#' @description This is a wrapper around the [PipeOpDistrCompositor] pipe operation, which
#' simplifies graph creation.
#' @param learner [LearnerSurv] object for which a `distr` is composed (or over-written).
#' @param estimator One of `kaplan` (default) or `nelson`, corresponding to the Kaplan-Meier and
#' Nelson-Aalen estimators respectively. Used to estimate the baseline survival distribution.
#' Abbreviations allowed.
#' @param form One of `aft` (default), `ph`, or `po`, corresponding to accelerated failure time,
#' proportional hazards, and proportional odds respectively. Used to determine the form of the
#' composed survival distribution.
#' @param overwrite logical. If `FALSE` (default) then if the `learner` already has a `distr`,
#' the compositor does nothing. If `TRUE` then the `distr` is overwritten by the compositor if
#' already present, which may be required for changing the prediction `distr` from one model form
#' to another.
#' @param param_vals Additional parameters to pass to the `learner`.
#' @details For full details see [PipeOpDistrCompositor].
#' @return [mlr3pipelines::GraphLearner]
#' @examples
#' \dontrun{
#' library("mlr3")
#' library("mlr3pipelines")
#'
#' task = tgen("simsurv")$generate(20)
#' cox.distr = distrcompositor(
#'   learner = lrn("surv.coxph"),
#'   estimator = "kaplan",
#'   form = "aft")
#'
#' resample(task, cox.distr, rsmp("cv", folds = 2))$predictions()
#' }
#' @export
distrcompositor = function(learner, estimator = c("kaplan", "nelson"), form = c("aft", "ph", "po"),
  overwrite = FALSE, param_vals = list()) {

  pred = mlr3pipelines::po("learner", learner, param_vals = param_vals)

  base = match.arg(estimator)
  base = mlr3pipelines::po("learner", lrn(paste("surv", base, sep = ".")))

  compositor = mlr3pipelines::po("distrcompose", param_vals = list(form = match.arg(form),
                                                                   overwrite = overwrite))

  mlr3pipelines::GraphLearner$new(mlr3pipelines::`%>>%`(mlr3pipelines::gunion(list(base, pred)),
                                                        compositor))
}
