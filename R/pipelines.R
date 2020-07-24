#' @title Average Survival Predictions
#' @description This is a wrapper around the [PipeOpSurvAvg] pipe operation, which
#' simplifies graph creation.
#' @param learners `(list())` \cr
#' List of [LearnerSurv]s to average.
#' @param param_vals `(list())` \cr
#' Parameters, including weights, to pass to [PipeOpSurvAvg].
#' @details For full details see [PipeOpSurvAvg].
#' @return [mlr3pipelines::GraphLearner]
#' @examples
#' \dontrun{
#' library("mlr3")
#'
#' task = tgen("simsurv")$generate(5)
#' avg = surv_averager(
#'   learners = lrns(c("surv.kaplan", "surv.coxph")),
#'   param_vals = list(weights = c(0.1, 0.9))
#'  )
#' avg$train(task)$predict(task)
#' }
#' @export
pipeline_survaverage = function(learners, param_vals = list(), graph_learner = TRUE) {

  learners = mlr3pipelines::gunion(mlr3misc::map(learners,
                                                 function(.x) mlr3pipelines::po("learner", .x)))
  po = mlr3pipelines::po("survavg", param_vals = param_vals)

  gr = learners %>>% po

  if (graph_learner) {
    return(GraphLearner$new(gr))
  } else {
    return(gr)
  }
}


#' @title Bag Survival Predictions
#' @description This is a bagging graph based around the [PipeOpSurvAvg] pipe operation, which first
#' subsamples the data and then fits the same learner multiple times and aggregates the results.
#' @param learners `(list())` \cr
#' List of [LearnerSurv]s to average.
#' @param param_vals `(list())` \cr
#' Parameters, including weights, to pass to [PipeOpSurvAvg].
#' @details For full details see [PipeOpSurvAvg].
#' @return [mlr3pipelines::GraphLearner]
#' @examples
#' \dontrun{
#' library("mlr3")
#'
#' task = tgen("simsurv")$generate(5)
#' avg = surv_averager(
#'   learners = lrns(c("surv.kaplan", "surv.coxph")),
#'   param_vals = list(weights = c(0.1, 0.9))
#'  )
#' avg$train(task)$predict(task)
#' }
#' @export
pipeline_survbagging = function(learner, iterations = 10, frac = 0.7, weights = 1,
                                graph_learner = TRUE) {

  assertCount(iterations)
  assert_number(frac, lower = 0, upper = 1)

  graph = as_graph(learner)
  subs = po("subsample", param_vals = list(frac = frac)) %>>% graph
  subs_repls = pipeline_greplicate(subs, iterations)

  po = mlr3pipelines::po("survavg", param_vals = list(weights = weights))

  gr = subs_repls %>>% po

  if (graph_learner) {
    return(GraphLearner$new(gr))
  } else {
    return(gr)
  }
}

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
                                    response = FALSE, graph_learner = TRUE) {

  if (testCharacter(learner)) {
    warning("Passing a learner id is now deprecated. In the future please pass a constructed
            learner or graph instead.")
    learner = lrn(learner)
  }

  pred = as_graph(learner)

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

#' @rdname pipeline_crankcompositor
#' @export
crankcompositor = function(...) {
  warning("Deprecated, please now use pipeline_crankcompositor or ppl('crankcompositor', ...).")
  pipeline_crankcompositor(...)
}

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
#'
#' # alternatively as a graph
#' ppl("distrcompositor", learner = lrn("surv.coxph"))
#' }
#' @export
pipeline_distrcompositor = function(learner, estimator = c("kaplan", "nelson"),
                                    form = c("aft", "ph", "po"),
                                    overwrite = FALSE, graph_learner = TRUE) {

  if (testCharacter(learner)) {
    warning("Passing a learner id is now deprecated. In the future please pass a constructed
            learner or graph instead.")
    learner = lrn(learner)
  }

  pred = as_graph(learner)

  base = match.arg(estimator)
  base = po("learner", lrn(paste("surv", base, sep = ".")))

  compositor = po("distrcompose", param_vals = list(form = match.arg(form), overwrite = overwrite))

  gr = gunion(list(base, pred)) %>>% compositor

  if (graph_learner) {
    return(GraphLearner$new(gr))
  } else {
    return(gr)
  }
}

#' @rdname pipeline_distrcompositor
#' @export
distrcompositor = function(...) {
  warning("Deprecated, please now use pipeline_distrcompositor or ppl('distrcompositor', ...).")
  pipeline_distrcompositor(...)
}

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
pipeline_probregrcompositor = function(learner, dist = "Normal", graph_learner = TRUE) {

  pred = as_graph(learner)
  compositor = mlr3pipelines::po("probregr_compose", param_vals = list(dist = dist))

  gr = pred %>>% compositor

  if (graph_learner) {
    return(GraphLearner$new(gr))
  } else {
    return(gr)
  }
}
