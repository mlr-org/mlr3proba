#' @template pipeline
#' @templateVar title Survival Prediction Averaging
#' @templateVar pipeop [PipeOpSurvAvg]
#' @templateVar id survaverager
#' @param learners `(list())` \cr
#' List of [LearnerSurv]s to average.
#' @param param_vals `(list())` \cr
#' Parameters, including weights, to pass to [PipeOpSurvAvg].
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#' library("mlr3")
#' library("mlr3pipelines")
#'
#' task = tsk("rats")
#' pipe = ppl(
#'   "survaverager",
#'   learners = lrns(c("surv.kaplan", "surv.coxph")),
#'   param_vals = list(weights = c(0.1, 0.9)),
#'   graph_learner = FALSE
#'  )
#' pipe$train(task)
#' pipe$predict(task)
#' }
#' }
pipeline_survaverager = function(learners, param_vals = list(), graph_learner = FALSE) {

  learners = mlr3pipelines::gunion(mlr3misc::map(learners, mlr3pipelines::as_graph))
  po = mlr3pipelines::po("survavg", param_vals = param_vals)

  gr = mlr3pipelines::`%>>%`(learners, po)

  if (graph_learner) {
    return(mlr3pipelines::GraphLearner$new(gr))
  } else {
    return(gr)
  }
}


#' @template pipeline
#' @templateVar title Survival Prediction Averaging
#' @templateVar pipeop [PipeOpSubsample][mlr3pipelines::PipeOpSubsample] and [PipeOpSurvAvg]
#' @templateVar id survbagging
#' @template param_pipeline_learner
#' @param iterations `integer(1)`\cr
#' Number of bagging iterations. Defaults to 10.
#' @param frac `numeric(1)`\cr
#' Percentage of rows to keep during subsampling. See
#' [PipeOpSubsample][mlr3pipelines::PipeOpSubsample] for more information. Defaults to 0.7.
#' @param avg `logical(1)`\cr
#' If `TRUE` (default) predictions are aggregated with [PipeOpSurvAvg], otherwise returned
#' as multiple predictions. Can only be `FALSE` if `graph_learner = FALSE`.
#' @param weights `numeric()` \cr
#' Weights for model avering, ignored if `avg = FALSE`. Default is uniform weighting,
#' see [PipeOpSurvAvg].
#' @details Bagging (Bootstrap AGGregatING) is the process of bootstrapping data and aggregating
#' the final predictions. Bootstrapping splits the data into `B` smaller datasets of a given size
#' and is performed with [PipeOpSubsample][mlr3pipelines::PipeOpSubsample]. Aggregation is
#' the sample mean of deterministic predictions and a
#' [MixtureDistribution][distr6::MixtureDistribution] of distribution predictions. This can be
#' further enhanced by using a weighted average by supplying `weights`.
#' @return [mlr3pipelines::GraphLearner]
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#' library("mlr3")
#' library("mlr3pipelines")
#'
#' task = tsk("rats")
#' pipe = ppl(
#'   "survbagging",
#'   learner = lrn("surv.coxph"),
#'   iterations = 5,
#'   graph_learner = FALSE
#'  )
#' pipe$train(task)
#' pipe$predict(task)
#' }
#' }
pipeline_survbagging = function(learner, iterations = 10, frac = 0.7, avg = TRUE, weights = 1,
                                graph_learner = FALSE) {

  assertCount(iterations)
  assert_number(frac, lower = 0, upper = 1)

  graph = mlr3pipelines::as_graph(learner)
  subs = mlr3pipelines::`%>>%`(mlr3pipelines::po("subsample", param_vals = list(frac = frac)),
                               graph)
  subs_repls = mlr3pipelines::pipeline_greplicate(subs, iterations)

  if (!avg) {
    return(subs_repls)
  } else {
    po = mlr3pipelines::po("survavg", param_vals = list(weights = weights))

    gr = mlr3pipelines::`%>>%`(subs_repls, po)

    if (graph_learner) {
      return(mlr3pipelines::GraphLearner$new(gr))
    } else {
      return(gr)
    }
  }
}

#' @template pipeline
#' @templateVar title Estimate Survival crank Predict Type
#' @templateVar pipeop [PipeOpCrankCompositor]
#' @templateVar id crankcompositor
#' @template param_pipeline_learner
#' @param method `character(1)`\cr
#' One of `mean` (default), `mode`, or `median`; abbreviations allowed. Used to determine
#' how `crank` is estimated from the predicted `distr`.
#' @param which `integer(1)`\cr
#' If `method = "mode"` then specifies which mode to use if multi-modal, default
#' is the first.
#' @param response `logical(1)`\cr
#' If `TRUE` then the `response` predict type is also estimated with the same values as `crank`.
#' @param overwrite `logical(1)`\cr
#' If `TRUE` then existing `response` and `crank` predict types are overwritten.
#' @param ... `ANY`\cr
#' For use with `crankcompositor`, now deprecated.
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#' library("mlr3")
#' library("mlr3pipelines")
#'
#' task = tsk("rats")
#' pipe = ppl(
#'   "crankcompositor",
#'   learner = lrn("surv.coxph"),
#'   method = "median"
#' )
#' pipe$train(task)
#' pipe$predict(task)
#' }
#' }
pipeline_crankcompositor = function(learner, method = c("mean", "median", "mode"), which = NULL,
                                    response = FALSE, overwrite = FALSE, graph_learner = FALSE) {

  if (testCharacter(learner)) {
    warning("Passing a learner id is now deprecated. In the future please pass a constructed
            learner or graph instead.")
    learner = lrn(learner)
  }

  pred = mlr3pipelines::as_graph(learner)

  pv = list(method = match.arg(method), response = response, overwrite = overwrite)
  if (!is.null(which)) {
    pv$which = which
  }
  compositor = mlr3pipelines::po("crankcompose", param_vals = pv)

  gr = mlr3pipelines::`%>>%`(pred, compositor)

  if (graph_learner) {
    return(mlr3pipelines::GraphLearner$new(gr))
  } else {
    return(gr)
  }
}

#' @rdname mlr_graphs_crankcompositor
#' @export
crankcompositor = function(...) {
  warning("Deprecated, please now use pipeline_crankcompositor or ppl('crankcompositor', ...).
          graph_learner will also be FALSE by default.")
  if ("graph_learner" %nin% names(list(...))) {
    pipeline_crankcompositor(graph_learner = TRUE, ...)
  } else {
    pipeline_crankcompositor(...)
  }
}


#' @template pipeline
#' @templateVar title Estimate Survival distr Predict Type
#' @templateVar pipeop [PipeOpDistrCompositor]
#' @templateVar id distrcompositor
#' @template param_pipeline_learner
#' @param estimator `character(1)`\cr
#' One of `kaplan` (default) or `nelson`, corresponding to the Kaplan-Meier and
#' Nelson-Aalen estimators respectively. Used to estimate the baseline survival distribution.
#' @param form `character(1)`\cr
#' One of `aft` (default), `ph`, or `po`, corresponding to accelerated failure time,
#' proportional hazards, and proportional odds respectively. Used to determine the form of the
#' composed survival distribution.
#' @param overwrite `logical(1)`\cr
#' If `FALSE` (default) then if the `learner` already has a `distr`, the compositor does nothing.
#' If `TRUE` then the `distr` is overwritten by the compositor if
#' already present, which may be required for changing the prediction `distr` from one model form
#' to another.
#' @param ... `ANY`\cr
#' For use with `distrcompositor`, now deprecated.
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE) &&
#'  requireNamespace("rpart", quietly = TRUE)) {
#' library("mlr3")
#' library("mlr3pipelines")
#'
#' task = tsk("rats")
#' pipe = ppl(
#'   "distrcompositor",
#'   learner = lrn("surv.rpart"),
#'   estimator = "kaplan",
#'   form = "ph"
#' )
#' pipe$train(task)
#' pipe$predict(task)
#' }
#' }
pipeline_distrcompositor = function(learner, estimator = c("kaplan", "nelson"),
                                    form = c("aft", "ph", "po"),
                                    overwrite = FALSE, graph_learner = FALSE) {

  if (testCharacter(learner)) {
    warning("Passing a learner id is now deprecated. In the future please pass a constructed
            learner or graph instead.")
    learner = lrn(learner)
  }

  pred = mlr3pipelines::as_graph(learner)

  base = match.arg(estimator)
  base = mlr3pipelines::po("learner", lrn(paste("surv", base, sep = ".")))

  compositor = mlr3pipelines::po("distrcompose", param_vals = list(form = match.arg(form), overwrite = overwrite))

  gr = mlr3pipelines::`%>>%`(mlr3pipelines::gunion(list(base, pred)), compositor)

  if (graph_learner) {
    return(mlr3pipelines::GraphLearner$new(gr))
  } else {
    return(gr)
  }
}

#' @rdname mlr_graphs_distrcompositor
#' @export
distrcompositor = function(...) {
  warning("Deprecated, please now use pipeline_distrcompositor or ppl('distrcompositor', ...).
          graph_learner will also be FALSE by default.")
  if ("graph_learner" %nin% names(list(...))) {
    pipeline_distrcompositor(graph_learner = TRUE, ...)
  } else {
    pipeline_distrcompositor(...)
  }
}


#' @template pipeline
#' @templateVar title Estimate Regression distr Predict Type
#' @templateVar pipeop [PipeOpProbregrCompositor]
#' @templateVar id probregrcompositor
#' @template param_pipeline_learner_regr
#' @param learner_se `[mlr3::Learner]|[mlr3pipelines::PipeOp]` \cr
#' Optional [LearnerRegr][mlr3::LearnerRegr] with predict_type `se` to estimate the standard
#' error. If left `NULL` then `learner` must have `se` in predict_types.
#' @param dist `character(1)`\cr
#' Location-scale distribution to use for composition.
#' Current possibilities are' `"Cauchy", "Gumbel", "Laplace", "Logistic", "Normal` (default).
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE) &&
#'   requireNamespace("rpart", quietly = TRUE)) {
#' library("mlr3")
#' library("mlr3pipelines")
#'
#' task = tsk("boston_housing")
#'
#' # method 1 - one learner for response and se
#' pipe = ppl(
#'   "probregrcompositor",
#'   learner = lrn("regr.featureless", predict_type = "se"),
#'   dist = "Normal"
#' )
#' pipe$train(task)
#' pipe$predict(task)
#'
#' # method 2 - one learner for response and one for se
#' pipe = ppl(
#'   "probregrcompositor",
#'   learner = lrn("regr.rpart"),
#'   learner_se = lrn("regr.featureless", predict_type = "se"),
#'   dist = "Logistic"
#' )
#' pipe$train(task)
#' pipe$predict(task)
#' }
#' }
pipeline_probregrcompositor = function(learner, learner_se = NULL, dist = "Normal",
                                       graph_learner = FALSE) {

  gr = mlr3pipelines::Graph$new()$add_pipeop(mlr3pipelines::po("compose_probregr", param_vals = list(dist = dist)))

  if (is.null(learner_se)) {
    learner$predict_type = "se"
    gr$add_pipeop(mlr3pipelines::po("learner", learner, id = "response_learner"))$
      add_edge("response_learner", "compose_probregr", dst_channel = "input_response")$
      add_edge("response_learner", "compose_probregr", dst_channel = "input_se")
  } else {
    learner_se$predict_type = "se"
    gr$add_pipeop(mlr3pipelines::po("learner", learner, id = "response_learner"))$
      add_pipeop(mlr3pipelines::po("learner", learner_se, id = "se_learner"))$
      add_edge("response_learner", "compose_probregr", dst_channel = "input_response")$
      add_edge("se_learner", "compose_probregr", dst_channel = "input_se")
  }

  if (graph_learner) {
    return(mlr3pipelines::GraphLearner$new(gr))
  } else {
    return(gr)
  }
}

#' @name mlr_graphs_survtoregr
#' @title Survival to Regression Reduction Pipeline
#' @description Wrapper around multiple [PipeOp][mlr3pipelines::PipeOp]s to help in creation
#' of complex survival to reduction methods. Three reductions are currently implemented,
#' see details.
#' @details
#' Three reduction strategies are implemented, these are:
#'
#' \enumerate{
#' \item Survival to Deterministic Regression A
#' \enumerate{
#' \item [PipeOpTaskSurvRegr] Converts [TaskSurv] to [TaskRegr][mlr3::TaskRegr].
#' \item A [LearnerRegr] is fit and predicted on the new `TaskRegr`.
#' \item [PipeOpPredRegrSurv] transforms the resulting [PredictionRegr][mlr3::PredictionRegr]
#' to [PredictionSurv].
#' \item Optionally: [PipeOpDistrCompositor] is used to compose a `distr` predict_type from the
#' predicted `response` predict_type.
#' }
#' \item Survival to Probabilistic Regression
#' \enumerate{
#' \item [PipeOpTaskSurvRegr] Converts [TaskSurv] to [TaskRegr][mlr3::TaskRegr].
#' \item A [LearnerRegr] is fit on the new `TaskRegr` to predict `response`, optionally a second
#' `LearnerRegr` can be fit to predict `se`.
#' \item [PipeOpProbregrCompositor] composes a `distr` prediction from the learner(s).
#' \item [PipeOpPredRegrSurv] transforms the resulting [PredictionRegr][mlr3::PredictionRegr]
#' to [PredictionSurv].
#' }
#' \item Survival to Deterministic Regression B
#' \enumerate{
#' \item [PipeOpLearnerCV][mlr3pipelines::PipeOpLearnerCV] cross-validates and makes predictions from
#' a linear [LearnerSurv] with `lp` predict type on the original [TaskSurv].
#' \item [PipeOpTaskSurvRegr] transforms the `lp` predictions into the target of a
#' [TaskRegr][mlr3::TaskRegr] with the same features as the original [TaskSurv].
#' \item A [LearnerRegr] is fit and predicted on the new `TaskRegr`.
#' \item [PipeOpPredRegrSurv] transforms the resulting [PredictionRegr][mlr3::PredictionRegr]
#' to [PredictionSurv].
#' \item Optionally: [PipeOpDistrCompositor] is used to compose a `distr` predict_type from the
#' predicted `lp` predict_type.
#' }
#' }
#'
#' Interpretation:
#'
#' 1. Once a dataset has censoring removed (by a given method) then a regression
#' learner can predict the survival time as the `response`.
#' 2. This is a very similar reduction to the first method with the main difference
#' being the distribution composition. In the first case this is composed in a survival framework
#' by assuming a linear model form and baseline hazard estimator, in the second case the
#' composition is in a regression framework. The latter case could result in problematic negative
#' predictions and should therefore be interpreted with caution, however a wider choice of
#' distributions makes it a more flexible composition.
#' 3. This is a rarer use-case that bypasses censoring not be removing it but instead
#' by first predicting the linear predictor from a survival model and fitting a regression
#' model on these predictions. The resulting regression predictions can then be viewed as the linear
#' predictors of the new data, which can ultimately be composed to a distribution.
#'
#' @param method `integer(1)`\cr
#' Reduction method to use, corresponds to those in `details`. Default is `1`.
#' @param regr_learner [LearnerRegr][mlr3::LearnerRegr]\cr
#' Regression learner to fit to the transformed [TaskRegr][mlr3::TaskRegr]. If `regr_se_learner` is
#' `NULL` in method `2`, then `regr_learner` must have `se` predict_type.
#' @param distrcompose `logical(1)`\cr
#' For methods `1` and `3` if `TRUE` (default) then [PipeOpDistrCompositor] is utilised to
#' transform the deterministic predictions to a survival distribution.
#' @param distr_estimator [LearnerSurv]\cr
#' For methods `1` and `3` if `distrcompose = TRUE` then specifies the learner to estimate the
#' baseline hazard, must have predict_type `distr`.
#' @param regr_se_learner [LearnerRegr][mlr3::LearnerRegr]\cr
#' For method `2` if `regr_learner` is not used to predict the `se` then a `LearnerRegr` with `se`
#' predict_type must be provided.
#' @param surv_learner [LearnerSurv]\cr
#' For method `3`, a [LearnerSurv] with `lp` predict type to estimate linear predictors.
#' @param survregr_params `list()`\cr
#' Parameters passed to [PipeOpTaskSurvRegr], default are survival to regression transformation
#' via `ipcw`, with weighting determined by Kaplan-Meier and no additional penalty for censoring.
#' @param distrcompose_params `list()`\cr
#' Parameters passed to [PipeOpDistrCompositor], default is accelerated failure time model form.
#' @param probregr_params `list()`\cr
#' Parameters passed to [PipeOpProbregrCompositor], default is [Normal][distr6::Normal]
#' distribution for composition.
#' @param learnercv_params `list()`\cr
#' Parameters passed to [PipeOpLearnerCV][mlr3pipelines::PipeOpLearnerCV], default is to use
#' insampling.
#' @param graph_learner `logical(1)`\cr
#' If `TRUE` returns wraps the [Graph][mlr3pipelines::Graph] as a
#' [GraphLearner][mlr3pipelines::GraphLearner] otherwise (default) returns as a `Graph`.
#'
#' @return [mlr3pipelines::Graph] or [mlr3pipelines::GraphLearner]
#' @family pipelines
#'
#' @examples
#' \dontrun{
#' if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
#' library("mlr3")
#' library("mlr3pipelines")
#'
#' task = tsk("rats")
#'
#' # method 1 with censoring deletion, compose to distribution
#' pipe = ppl(
#'   "survtoregr",
#'   method = 1,
#'   regr_learner = lrn("regr.featureless"),
#'   distrcompose = TRUE,
#'   survregr_params = list(method = "delete")
#' )
#' pipe$train(task)
#' pipe$predict(task)
#'
#' # method 2 with censoring imputation (mrl), one regr learner
#' pipe = ppl(
#'   "survtoregr",
#'   method = 2,
#'   regr_learner = lrn("regr.featureless", predict_type = "se"),
#'   survregr_params = list(method = "mrl")
#' )
#' pipe$train(task)
#' pipe$predict(task)
#'
#' # method 3 with censoring omission and no composition, insample resampling
#' pipe = ppl(
#'   "survtoregr",
#'   method = 3,
#'   regr_learner = lrn("regr.featureless"),
#'   distrcompose = FALSE,
#'   surv_learner = lrn("surv.coxph"),
#'   survregr_params = list(method = "omission")
#' )
#' pipe$train(task)
#' pipe$predict(task)
#' }
#'}
#' @export
pipeline_survtoregr = function(method = 1, regr_learner = lrn("regr.featureless"),
                          distrcompose = TRUE, distr_estimator = lrn("surv.kaplan"),
                          regr_se_learner = NULL,
                          surv_learner = lrn("surv.coxph"),
                          survregr_params = list(method = "ipcw", estimator = "kaplan", alpha = 1),
                          distrcompose_params = list(form = "aft"),
                          probregr_params = list(dist = "Normal"),
                          learnercv_params = list(resampling.method = "insample"),
                          graph_learner = FALSE) {


  if (method == 1) {
    gr = mlr3pipelines::Graph$new()$
      add_pipeop(mlr3pipelines::po("nop", id = "task_surv"))$
      add_pipeop(mlr3pipelines::po("trafotask_survregr", param_vals = survregr_params))$
      add_pipeop(mlr3pipelines::po("learner", regr_learner, id = "regr_learner"))$
      add_pipeop(mlr3pipelines::po("trafopred_regrsurv"))$
      add_edge("task_surv", "trafotask_survregr", dst_channel = "input")$
      add_edge("trafotask_survregr", "regr_learner")$
      add_edge("regr_learner", "trafopred_regrsurv", dst_channel = "pred")$
      add_edge("task_surv", "trafopred_regrsurv", dst_channel = "task")

    if (distrcompose) {
      assert("distr" %in% distr_estimator$predict_types)

      gr$add_pipeop(mlr3pipelines::po("learner", distr_estimator, id = "distr_estimator"))$
        add_pipeop(mlr3pipelines::po("compose_distr", param_vals = distrcompose_params))$
        add_edge("trafopred_regrsurv", dst_id = "compose_distr", dst_channel = "pred")$
        add_edge("distr_estimator", dst_id = "compose_distr", dst_channel = "base")
    }

  } else if (method == 2) {

    gr = mlr3pipelines::Graph$new()$
      add_pipeop(mlr3pipelines::po("nop", id = "task_surv"))$
      add_pipeop(mlr3pipelines::po("trafotask_survregr", param_vals = survregr_params))$
      add_pipeop(mlr3pipelines::po("compose_probregr", param_vals = probregr_params))$
      add_pipeop(mlr3pipelines::po("trafopred_regrsurv"))$
      add_edge("compose_probregr", "trafopred_regrsurv", dst_channel = "pred")$
      add_edge("task_surv", "trafopred_regrsurv", dst_channel = "task")

    if (!is.null(regr_se_learner)) {
      regr_se_learner$predict_type = "se"

      gr$add_pipeop(mlr3pipelines::po("learner", regr_learner, id = "regr_learner"))$
        add_pipeop(mlr3pipelines::po("learner", regr_se_learner, id = "regr_se_learner"))$
        add_edge("trafotask_survregr", "regr_se_learner")$
        add_edge("regr_se_learner", "compose_probregr", dst_channel = "input_se")
    } else {
      regr_learner$predict_type = "se"
      gr$add_pipeop(mlr3pipelines::po("learner", regr_learner, id = "regr_learner"))$
        add_edge("regr_learner", "compose_probregr", dst_channel = "input_se")
    }

    gr$add_edge("trafotask_survregr", "regr_learner")$
      add_edge("regr_learner", "compose_probregr", dst_channel = "input_response")

  } else if (method == 3) {

    assert("lp" %in% surv_learner$predict_types)

    gr = mlr3pipelines::Graph$new()$
      add_pipeop(mlr3pipelines::po("nop", id = "task_surv_train"))$
      add_pipeop(mlr3pipelines::po("nop", id = "task_surv_predict"))$
      add_pipeop(mlr3pipelines::po("learner_cv", surv_learner, id = "surv_learner",
                    param_vals = learnercv_params))$
      add_pipeop(mlr3pipelines::po("trafotask_survregr", method = "reorder", target = "surv_learner.lp"))$
      add_pipeop(mlr3pipelines::po("learner", regr_learner, id = "regr_learner"))$
      add_pipeop(mlr3pipelines::po("trafopred_regrsurv", target_type = "lp"))$
      add_edge("surv_learner", "trafotask_survregr", dst_channel = "input")$
      add_edge("task_surv_train", "trafotask_survregr", dst_channel = "input_features")$
      add_edge("trafotask_survregr", "regr_learner")$
      add_edge("regr_learner", "trafopred_regrsurv", dst_channel = "pred")$
      add_edge("task_surv_predict", "trafopred_regrsurv", dst_channel = "task")

    if (distrcompose) {
      assert("distr" %in% distr_estimator$predict_types)

      gr$add_pipeop(mlr3pipelines::po("learner", distr_estimator, id = "distr_estimator"))$
        add_pipeop(mlr3pipelines::po("compose_distr", param_vals = distrcompose_params))$
        add_edge("trafopred_regrsurv", dst_id = "compose_distr", dst_channel = "pred")$
        add_edge("distr_estimator", dst_id = "compose_distr", dst_channel = "base")
    }
  }

  if (graph_learner) {
    return(mlr3pipelines::GraphLearner$new(gr))
  } else {
    return(gr)
  }
}
