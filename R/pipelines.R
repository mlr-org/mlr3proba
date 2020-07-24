#' @template pipeline
#' @templateVar title Average Survival Predictions
#' @templateVar pipeop PipeOpSurvAvg
#' @templateVar survaverager
#' @param learners `(list())` \cr
#' List of [LearnerSurv]s to average.
#' @param param_vals `(list())` \cr
#' Parameters, including weights, to pass to [PipeOpSurvAvg].
#' @examples
#' \dontrun{
#' library("mlr3")
#' library("mlr3pipelines")
#'
#' task = tgen("simsurv")$generate(5)
#' pipe = ppl(
#'   "survaverager",
#'   learners = lrns(c("surv.kaplan", "surv.coxph")),
#'   param_vals = list(weights = c(0.1, 0.9)),
#'   graph_learner = FALSE
#'  )
#' pipe$train(task)$predict(task)
#' }
#' @export
pipeline_survaverager = function(learners, param_vals = list(), graph_learner = FALSE) {

  learners = gunion(mlr3misc::map(learners, as_graph))
  po = po("survavg", param_vals = param_vals)

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
                                graph_learner = FALSE) {

  assertCount(iterations)
  assert_number(frac, lower = 0, upper = 1)

  graph = as_graph(learner)
  subs = po("subsample", param_vals = list(frac = frac)) %>>% graph
  subs_repls = pipeline_greplicate(subs, iterations)

  po = po("survavg", param_vals = list(weights = weights))

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
                                    response = FALSE, graph_learner = FALSE) {

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
  warning("Deprecated, please now use pipeline_crankcompositor or ppl('crankcompositor', ...).
          graph_learner will also be FALSE by default.")
  if (graph_learner %nin% names(list(...))) {
    pipeline_crankcompositor(graph_learner = TRUE, ...)
  } else {
    pipeline_crankcompositor(...)
  }
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
                                    overwrite = FALSE, graph_learner = FALSE) {

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
  warning("Deprecated, please now use pipeline_distrcompositor or ppl('distrcompositor', ...).
          graph_learner will also be FALSE by default.")
  if (graph_learner %nin% names(list(...))) {
    pipeline_distrcompositor(graph_learner = TRUE, ...)
  } else {
    pipeline_distrcompositor(...)
  }
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
pipeline_probregrcompositor = function(learner, dist = "Normal", graph_learner = FALSE) {

  pred = as_graph(learner)
  compositor = po("probregr_compose", param_vals = list(dist = dist))

  gr = pred %>>% compositor

  if (graph_learner) {
    return(GraphLearner$new(gr))
  } else {
    return(gr)
  }
}

pipeline_survregr = function(method = 1, graph_learner = FALSE,
                             regr_learner = "regr.featureless", distrcompose = TRUE,
                             distr_estimator = "surv.kaplan", regr_se_learner = NULL,
                             surv_learner = "surv.coxph") {

  if (method == 1) {
    gr = Graph$new()$
      add_pipeop(po("nop", phase = "predict", id = "task_surv"))$
      add_pipeop(po("trafotask_survregr"))$
      add_pipeop(po("learner", lrn(regr_learner), id = "regr_learner"))$
      add_pipeop(po("trafopred_regrsurv"))$
      add_edge("trafotask_survregr", "regr_learner")$
      add_edge("regr_learner", "trafopred_regrsurv", dst_channel = "pred")$
      add_edge("task_surv", "trafopred_regrsurv", dst_channel = "task")

    if (distrcompose) {
      gr$add_pipeop(po("learner", lrn(distr_estimator), id = "distr_estimator"))$
        add_pipeop(po("compose_distr"))$
        add_edge("trafopred_regrsurv", dst_id = "compose_distr", dst_channel = "pred")$
        add_edge("distr_estimator", dst_id = "compose_distr", dst_channel = "base")
    }

  } else if (method == 2) {

    gr = Graph$new()$
      add_pipeop(po("nop", phase = "predict", id = "task_surv"))$
      add_pipeop(po("trafotask_survregr"))$
      add_pipeop(po("compose_probregr"))$
      add_pipeop(po("trafopred_regrsurv"))$
      add_edge("compose_probregr", "trafopred_regrsurv", dst_channel = "pred")$
      add_edge("task_surv", "trafopred_regrsurv", dst_channel = "task")

    if (!is.null(regr_se_learner)) {
      gr$add_pipeop(po("learner", lrn(regr_learner), id = "regr_learner"))$
        add_pipeop(po("learner", lrn(regr_se_learner, predict_type = "se"), id = "regr_se_learner"))$
        add_edge("trafotask_survregr", "regr_se_learner")$
        add_edge("regr_se_learner", "compose_probregr", dst_channel = "input_se")
    } else {
      gr$add_pipeop(po("learner", lrn(regr_learner, predict_type = "se"), id = "regr_learner"))$
        add_edge("regr_learner", "compose_probregr", dst_channel = "input_se")
    }

    gr$add_edge("trafotask_survregr", "regr_learner")$
      add_edge("regr_learner", "compose_probregr", dst_channel = "input_response")

  } else if (method == 3) {
    surv_learner = lrn(surv_learner)
    assert("lp" %in% surv_learner$predict_types)

    gr = Graph$new()$
      add_pipeop(po("nop", phase = "both", id = "task_surv_train"))$
      add_pipeop(po("nop", phase = "predict", id = "task_surv_predict"))$
      add_pipeop(po("learner_cv", surv_learner, id = "surv_learner"))$
      add_pipeop(po("trafotask_survregr", method = "reorder", target = "surv_learner.lp"))$
      add_pipeop(po("learner", lrn(regr_learner), id = "regr_learner"))$
      add_pipeop(po("trafopred_regrsurv", target_type = "lp"))$
      add_edge("surv_learner", "trafotask_survregr", dst_channel = "input")$
      add_edge("task_surv_train", "trafotask_survregr", dst_channel = "input_features")$
      add_edge("trafotask_survregr", "regr_learner")$
      add_edge("regr_learner", "trafopred_regrsurv", dst_channel = "pred")$
      add_edge("task_surv_predict", "trafopred_regrsurv", dst_channel = "task")

    if (distrcompose) {
      gr$add_pipeop(po("learner", lrn(distr_estimator), id = "distr_estimator"))$
        add_pipeop(po("compose_distr"))$
        add_edge("trafopred_regrsurv", dst_id = "compose_distr", dst_channel = "pred")$
        add_edge("distr_estimator", dst_id = "compose_distr", dst_channel = "base")
    }
  }

  if (graph_learner) {
    return(GraphLearner$new(gr))
  } else {
    return(gr)
  }
}
