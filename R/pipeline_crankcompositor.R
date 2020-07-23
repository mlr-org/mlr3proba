#' @include PipeOpCrankCompositor.R
#'
#' @title Compose a Crank Predict Type for Survival Learners
#' @description This is a wrapper around the [PipeOpCrankCompositor] pipe operation, which
#' simplifies graph creation.
#' @param learner [LearnerSurv] object for which a `crank` is composed (or over-written)
#' @param method One of `mean`, `mode`, or `median`; abbreviations allowed. Used to determine
#' how `crank` is estimated from the predicted `distr`. Default is `mean`.
#' @param which If `method = "mode"` then specifies which mode to use if multi-modal, default
#' is the first.
#' @param response If `TRUE` then the `response` predict type is imputed with the same values
#' as `crank`.
#' @param param_vals Additional parameters to pass to the `learner`.
#' @details For full details see [PipeOpCrankCompositor].
#' @return [mlr3pipelines::GraphLearner]
#' @examples
#' \dontrun{
#' library(mlr3)
#' library(mlr3pipelines)
#'
#' task = tgen("simsurv")$generate(20)
#' cox.crank = crankcompositor(
#'   learner = lrn("surv.coxph"),
#'   method = "median")
#' resample(task, cox.crank, rsmp("cv", folds = 2))$predictions()
#' }
#' @export
pipeline_crankcompositor = function(learner, method = c("mean", "median", "mode"), which = NULL,
                           response = FALSE, param_vals = list(), graph_learner = TRUE) {
  assert("distr" %in% learner$predict_types)

  pred = po("learner", learner, param_vals = param_vals)
  pv = list(method = match.arg(method), response = response)
  if (!is.null(which)) {
    pv$which = which
  }
  compositor = po("crankcompose", param_vals = pv)

  gr = pred %>>% compositor

  if (graph_learner) {
    return(GraphLearner$new(gr))
  } else {
    return(gr)
  }
}

crankcompositor = function(...) {
  warning("Deprecated, please now use pipeline_crankcompositor or ppl('crankcompositor', ...).")
  pipeline_crankcompositor(...)
}
