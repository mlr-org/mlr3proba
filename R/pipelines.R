#' @template pipeline
#' @templateVar title Survival Prediction Averaging
#' @templateVar pipeop [PipeOpSurvAvg]
#' @templateVar id survaverager
#'
#' @param learners `(list())` \cr
#' List of [LearnerSurv]s to average.
#' @param param_vals `(list())` \cr
#' Parameters, including weights, to pass to [PipeOpSurvAvg].
#'
#' @examplesIf mlr3misc::require_namespaces(c("mlr3pipelines"), quietly = TRUE)
#' \dontrun{
#'   library("mlr3")
#'   library("mlr3pipelines")
#'
#'   task = tsk("rats")
#'   pipe = ppl(
#'     "survaverager",
#'     learners = lrns(c("surv.kaplan", "surv.coxph")),
#'     param_vals = list(weights = c(0.1, 0.9)),
#'     graph_learner = FALSE
#'   )
#'   pipe$train(task)
#'   pipe$predict(task)
#' }
pipeline_survaverager = function(learners, param_vals = list(), graph_learner = FALSE) {
  learners = mlr3pipelines::gunion(map(learners, mlr3pipelines::as_graph))
  po = mlr3pipelines::po("survavg", param_vals = param_vals)

  gr = mlr3pipelines::`%>>%`(learners, po)

  if (graph_learner) {
    gr = mlr3pipelines::GraphLearner$new(gr)
  }

  gr
}

#' @template pipeline
#' @templateVar title Survival Prediction Averaging
#' @templateVar pipeop [PipeOpSubsample][mlr3pipelines::PipeOpSubsample] and [PipeOpSurvAvg]
#' @templateVar id survbagging
#' @template param_pipeline_learner
#'
#' @param iterations (`integer(1)`)\cr
#' Number of bagging iterations. Defaults to 10.
#' @param frac (`numeric(1)`)\cr
#' Percentage of rows to keep during subsampling. See
#' [PipeOpSubsample][mlr3pipelines::PipeOpSubsample] for more information. Defaults to 0.7.
#' @param avg (`logical(1)`)\cr
#' If `TRUE` (default) predictions are aggregated with [PipeOpSurvAvg], otherwise returned
#' as multiple predictions. Can only be `FALSE` if `graph_learner = FALSE`.
#' @param weights (`numeric()`)\cr
#' Weights for model avering, ignored if `avg = FALSE`. Default is uniform weighting,
#' see [PipeOpSurvAvg].
#'
#' @details Bagging (Bootstrap AGGregatING) is the process of bootstrapping data and aggregating
#' the final predictions. Bootstrapping splits the data into `B` smaller datasets of a given size
#' and is performed with [PipeOpSubsample][mlr3pipelines::PipeOpSubsample]. Aggregation is
#' the sample mean of deterministic predictions and a
#' [MixtureDistribution][distr6::MixtureDistribution] of distribution predictions. This can be
#' further enhanced by using a weighted average by supplying `weights`.
#'
#' @examplesIf mlr3misc::require_namespaces(c("mlr3pipelines"), quietly = TRUE)
#' \dontrun{
#'   library("mlr3")
#'   library("mlr3pipelines")
#'
#'   task = tsk("rats")
#'   pipe = ppl(
#'     "survbagging",
#'     learner = lrn("surv.coxph"),
#'     iterations = 5,
#'     graph_learner = FALSE
#'   )
#'   pipe$train(task)
#'   pipe$predict(task)
#' }
pipeline_survbagging = function(learner, iterations = 10, frac = 0.7, avg = TRUE, weights = 1,
  graph_learner = FALSE) {

  assert_count(iterations)
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
      gr = mlr3pipelines::GraphLearner$new(gr)
    }

    gr
  }
}

#' @template pipeline
#' @templateVar title Estimate Survival crank Predict Type
#' @templateVar pipeop [PipeOpCrankCompositor]
#' @templateVar id crankcompositor
#' @template param_pipeline_learner
#'
#' @param method (`character(1)`)\cr
#' Determines what method should be used to produce a continuous ranking from the distribution.
#' Currently only `mort` is supported, which is the sum of the cumulative hazard, also called *expected/ensemble mortality*, see Ishwaran et al. (2008).
#' For more details, see [get_mortality()].
#' @param overwrite (`logical(1)`)\cr
#' If `FALSE` (default) and the prediction already has a `crank` prediction, then the compositor returns the input prediction unchanged.
#' If `TRUE`, then the `crank` will be overwritten.
#'
#' @examplesIf mlr3misc::require_namespaces(c("mlr3pipelines"), quietly = TRUE)
#' \dontrun{
#'   library("mlr3")
#'   library("mlr3pipelines")
#'
#'   task = tsk("lung")
#'   part = partition(task)
#'
#'   # change the crank prediction type of a Cox's model predictions
#'   grlrn = ppl(
#'     "crankcompositor",
#'     learner = lrn("surv.coxph"),
#'     method = "mort",
#'     overwrite = TRUE,
#'     graph_learner = TRUE
#'   )
#'   grlrn$train(task, part$train)
#'   grlrn$predict(task, part$test)
#' }
pipeline_crankcompositor = function(learner, method = c("mort"),
                                    overwrite = FALSE, graph_learner = FALSE) {
  assert_learner(learner, task_type = "surv")
  assert_choice(method, choices = c("mort"))
  assert_logical(overwrite)
  assert_logical(graph_learner)

  pred = mlr3pipelines::as_graph(learner)

  pv = list(method = method, overwrite = overwrite)
  compositor = mlr3pipelines::po("crankcompose", param_vals = pv)

  gr = mlr3pipelines::`%>>%`(pred, compositor)

  if (graph_learner) {
    gr = mlr3pipelines::GraphLearner$new(gr)
  }

  gr
}

#' @template pipeline
#' @templateVar title Estimate Survival Time/Response Predict Type
#' @templateVar pipeop [PipeOpResponseCompositor]
#' @templateVar id responsecompositor
#' @template param_pipeline_learner
#'
#' @param method (`character(1)`)\cr
#' Determines what method should be used to produce a survival time (response) from the survival distribution.
#' Available methods are `"rmst"` and `"median"`, corresponding to the *restricted mean survival time* and the *median survival time* respectively.
#' @param tau (`numeric(1)`)\cr
#' Determines the time point up to which we calculate the restricted mean survival time (works only for the `"rmst"` method).
#' If `NULL` (default), all the available time points in the predicted survival distribution will be used.
#' @param add_crank (`logical(1)`)\cr
#' If `TRUE` then `crank` predict type will be set as `-response` (as higher survival times correspond to lower risk).
#' Works only if `overwrite` is `TRUE`.
#' @param overwrite (`logical(1)`)\cr
#' If `FALSE` (default) and the prediction already has a `response` prediction, then the compositor returns the input prediction unchanged.
#' If `TRUE`, then the `response` (and the `crank`, if `add_crank` is `TRUE`) will be overwritten.
#'
#' @examplesIf mlr3misc::require_namespaces(c("mlr3pipelines"), quietly = TRUE)
#' \dontrun{
#'   library("mlr3")
#'   library("mlr3pipelines")
#'
#'   task = tsk("lung")
#'   part = partition(task)
#'
#'   # add survival time prediction type to the predictions of a Cox model
#'   grlrn = ppl(
#'     "responsecompositor",
#'     learner = lrn("surv.coxph"),
#'     method = "rmst",
#'     overwrite = TRUE,
#'     graph_learner = TRUE
#'   )
#'   grlrn$train(task, part$train)
#'   grlrn$predict(task, part$test)
#' }
pipeline_responsecompositor = function(learner, method = "rmst", tau = NULL,
                                       add_crank = FALSE, overwrite = FALSE,
                                       graph_learner = FALSE) {
  assert_learner(learner, task_type = "surv")
  assert_choice(method, choices = c("rmst", "median"))
  assert_number(tau, null.ok = TRUE, lower = 0)
  assert_logical(add_crank)
  assert_logical(overwrite)
  assert_logical(graph_learner)

  pred = mlr3pipelines::as_graph(learner)

  pv = list(method = method, tau = tau, add_crank = add_crank,
            overwrite = overwrite)
  compositor = mlr3pipelines::po("responsecompose", param_vals = pv)

  gr = mlr3pipelines::`%>>%`(pred, compositor)

  if (graph_learner) {
    gr = mlr3pipelines::GraphLearner$new(gr)
  }

  gr
}

#' @template pipeline
#' @templateVar title Estimate Survival distr Predict Type
#' @templateVar pipeop [PipeOpDistrCompositor] or [PipeOpBreslow]
#' @templateVar id distrcompositor
#' @template param_pipeline_learner
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param estimator (`character(1)`)\cr
#' One of `kaplan` (default), `nelson` or `breslow`, corresponding to the Kaplan-Meier,
#' Nelson-Aalen and [Breslow][breslow] estimators respectively.
#' Used to estimate the baseline survival distribution.
#' @param form (`character(1)`)\cr
#' One of `aft` (default), `ph`, or `po`, corresponding to accelerated failure time,
#' proportional hazards, and proportional odds respectively.
#' Used to determine the form of the composed survival distribution.
#' Ignored if estimator is `breslow`.
#' @param overwrite (`logical(1)`)\cr
#' If `FALSE` (default) then if the `learner` already has a `distr`, the compositor does nothing.
#' If `TRUE` then the `distr` is overwritten by the compositor if
#' already present, which may be required for changing the prediction `distr` from one model form
#' to another.
#'
#' @examplesIf mlr3misc::require_namespaces(c("mlr3pipelines"), quietly = TRUE)
#' \dontrun{
#'   library("mlr3pipelines")
#'
#'   # let's change the distribution prediction of Cox (Breslow-based) to an AFT form:
#'   task = tsk("rats")
#'   grlrn = ppl(
#'     "distrcompositor",
#'     learner = lrn("surv.coxph"),
#'     estimator = "kaplan",
#'     form = "aft",
#'     overwrite = TRUE,
#'     graph_learner = TRUE
#'   )
#'   grlrn$train(task)
#'   grlrn$predict(task)
#' }
pipeline_distrcompositor = function(learner, estimator = "kaplan", form = "aft",
  overwrite = FALSE, graph_learner = FALSE) {
  # some checks
  assert_choice(estimator, choices = c("kaplan", "nelson", "breslow"), null.ok = FALSE)
  assert_choice(form, choices = c("aft", "ph", "po"), null.ok = FALSE)
  assert_learner(learner, task_type = "surv")

  # make the pipeline Graph object
  if (estimator == "breslow") {
    gr = mlr3pipelines::as_graph(
      mlr3pipelines::po("breslowcompose", learner = learner, breslow.overwrite = overwrite)
    )
  } else {
    pred = mlr3pipelines::as_graph(learner)
    base = mlr3pipelines::po("learner",
      lrn(paste0("surv.", estimator), id = paste0("distrcompositor.", estimator)))

    compositor = mlr3pipelines::po("distrcompose", param_vals = list(form = form, overwrite = overwrite))

    gr = mlr3pipelines::`%>>%`(mlr3pipelines::gunion(list(base, pred)), compositor)
  }

  if (graph_learner) {
    gr = mlr3pipelines::GraphLearner$new(gr)
  }

  gr
}

#' @template pipeline
#' @templateVar title Estimate Regression distr Predict Type
#' @templateVar pipeop [PipeOpProbregr]
#' @templateVar id probregr
#' @template param_pipeline_learner_regr
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' @param learner_se `[mlr3::Learner]|[mlr3pipelines::PipeOp]` \cr
#' Optional [LearnerRegr][mlr3::LearnerRegr] with predict_type `se` to estimate the standard
#' error. If left `NULL` then `learner` must have `se` in predict_types.
#' @param dist (`character(1)`)\cr
#' Location-scale distribution to use for composition.
#' Current possibilities are' `"Cauchy", "Gumbel", "Laplace", "Logistic", "Normal", "Uniform"`. Default is `"Uniform"`.
#' @examplesIf mlr3misc::require_namespaces(c("mlr3pipelines", "rpart"), quietly = TRUE)
#' \dontrun{
#'   library("mlr3")
#'   library("mlr3pipelines")
#'
#'   task = tsk("boston_housing")
#'
#'   # method 1 - same learner for response and se
#'   pipe = ppl(
#'     "probregr",
#'     learner = lrn("regr.featureless", predict_type = "se"),
#'     dist = "Uniform"
#'   )
#'   pipe$train(task)
#'   pipe$predict(task)
#'
#'   # method 2 - different learners for response and se
#'   pipe = ppl(
#'     "probregr",
#'     learner = lrn("regr.rpart"),
#'     learner_se = lrn("regr.featureless", predict_type = "se"),
#'     dist = "Normal"
#'   )
#'   pipe$train(task)
#'   pipe$predict(task)
#' }
pipeline_probregr = function(learner, learner_se = NULL, dist = "Uniform",
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
    gr = mlr3pipelines::GraphLearner$new(gr)
  }

  gr
}

#' @name mlr_graphs_survtoregr
#' @title Survival to Regression Reduction Pipeline
#' @description Wrapper around multiple [PipeOp][mlr3pipelines::PipeOp]s to help in creation
#' of complex survival reduction methods. Three reductions are currently implemented,
#' see details.
#' `r lifecycle::badge("experimental")`
#'
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
#' }
#' \item Survival to Probabilistic Regression
#' \enumerate{
#' \item [PipeOpTaskSurvRegr] Converts [TaskSurv] to [TaskRegr][mlr3::TaskRegr].
#' \item A [LearnerRegr] is fit on the new `TaskRegr` to predict `response`, optionally a second
#' `LearnerRegr` can be fit to predict `se`.
#' \item [PipeOpProbregr] composes a `distr` prediction from the learner(s).
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
#' @param method (`integer(1)`)\cr
#' Reduction method to use, corresponds to those in `details`. Default is `1`.
#' @param regr_learner [LearnerRegr][mlr3::LearnerRegr]\cr
#' Regression learner to fit to the transformed [TaskRegr][mlr3::TaskRegr]. If `regr_se_learner` is
#' `NULL` in method `2`, then `regr_learner` must have `se` predict_type.
#' @param distrcompose (`logical(1)`)\cr
#' For method `3` if `TRUE` (default) then [PipeOpDistrCompositor] is utilised to
#' transform the deterministic predictions to a survival distribution.
#' @param distr_estimator [LearnerSurv]\cr
#' For methods `1` and `3` if `distrcompose = TRUE` then specifies the learner to estimate the
#' baseline hazard, must have predict_type `distr`.
#' @param regr_se_learner [LearnerRegr][mlr3::LearnerRegr]\cr
#' For method `2` if `regr_learner` is not used to predict the `se` then a `LearnerRegr` with `se`
#' predict_type must be provided.
#' @param surv_learner [LearnerSurv]\cr
#' For method `3`, a [LearnerSurv] with `lp` predict type to estimate linear predictors.
#' @param survregr_params (`list()`)\cr
#' Parameters passed to [PipeOpTaskSurvRegr], default are survival to regression transformation
#' via `ipcw`, with weighting determined by Kaplan-Meier and no additional penalty for censoring.
#' @param distrcompose_params (`list()`)\cr
#' Parameters passed to [PipeOpDistrCompositor], default is accelerated failure time model form.
#' @param probregr_params (`list()`)\cr
#' Parameters passed to [PipeOpProbregr], default is [Uniform][distr6::Uniform]
#' distribution for composition.
#' @param learnercv_params (`list()`)\cr
#' Parameters passed to [PipeOpLearnerCV][mlr3pipelines::PipeOpLearnerCV], default is to use
#' insampling.
#' @param graph_learner (`logical(1)`)\cr
#' If `TRUE` returns wraps the [Graph][mlr3pipelines::Graph] as a
#' [GraphLearner][mlr3pipelines::GraphLearner] otherwise (default) returns as a `Graph`.
#'
#' @examplesIf mlr3misc::require_namespaces(c("mlr3pipelines"), quietly = TRUE)
#' \dontrun{
#'   library("mlr3")
#'   library("mlr3pipelines")
#'
#'   task = tsk("rats")
#'
#'   # method 1 with censoring deletion, compose to distribution
#'   pipe = ppl(
#'     "survtoregr",
#'     method = 1,
#'     regr_learner = lrn("regr.featureless"),
#'     survregr_params = list(method = "delete")
#'   )
#'   pipe$train(task)
#'   pipe$predict(task)
#'
#'   # method 2 with censoring imputation (mrl), one regr learner
#'   pipe = ppl(
#'     "survtoregr",
#'     method = 2,
#'     regr_learner = lrn("regr.featureless", predict_type = "se"),
#'     survregr_params = list(method = "mrl")
#'   )
#'   pipe$train(task)
#'   pipe$predict(task)
#'
#'   # method 3 with censoring omission and no composition, insample resampling
#'   pipe = ppl(
#'     "survtoregr",
#'     method = 3,
#'     regr_learner = lrn("regr.featureless"),
#'     distrcompose = FALSE,
#'     surv_learner = lrn("surv.coxph"),
#'     survregr_params = list(method = "omission")
#'   )
#'   pipe$train(task)
#'   pipe$predict(task)
#' }
#' @export
pipeline_survtoregr = function(method = 1, regr_learner = lrn("regr.featureless"),
  distrcompose = TRUE, distr_estimator = lrn("surv.kaplan"),
  regr_se_learner = NULL,
  surv_learner = lrn("surv.coxph"),
  survregr_params = list(method = "ipcw", estimator = "kaplan", alpha = 1),
  distrcompose_params = list(form = "aft"),
  probregr_params = list(dist = "Uniform"),
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
        add_pipeop(mlr3pipelines::po("distrcompose", param_vals = distrcompose_params))$
        add_edge("trafopred_regrsurv", dst_id = "distrcompose", dst_channel = "pred")$
        add_edge("distr_estimator", dst_id = "distrcompose", dst_channel = "base")
    }
  }

  if (graph_learner) {
    gr = mlr3pipelines::GraphLearner$new(gr)
  }

  gr
}

#' @template pipeline
#' @templateVar pipeop [PipeOpTaskSurvClassifDiscTime] and [PipeOpPredClassifSurvDiscTime]
#' @templateVar id survtoclassif_disctime
#' @templateVar title Survival to Classification Reduction using Discrete Time
#'
#' @param learner [LearnerClassif][mlr3::LearnerClassif]\cr
#' Classification learner to fit the transformed [TaskClassif][mlr3::TaskClassif].
#' `learner` must have `predict_type` of type `"prob"`.
#' @param cut (`numeric()`)\cr
#' Split points, used to partition the data into intervals.
#' If unspecified, all unique event times will be used.
#' If `cut` is a single integer, it will be interpreted as the number of equidistant
#' intervals from 0 until the maximum event time.
#' @param max_time (`numeric(1)`)\cr
#' If cut is unspecified, this will be the last possible event time.
#' All event times after max_time will be administratively censored at max_time.
#' @param rhs (`character(1)`)\cr
#' Right-hand side of the formula to use with the learner.
#' All features of the task are available as well as `tend` the upper bounds
#' of the intervals created by `cut`.
#' If `rhs` is unspecified, the formula of the task will be used.
#'
#' @details
#' The pipeline consists of the following steps:
#'
#' 1. [PipeOpTaskSurvClassifDiscTime] Converts [TaskSurv] to a [TaskClassif][mlr3::TaskClassif].
#' 2. A [LearnerClassif] is fit and predicted on the new `TaskClassif`.
#' 3. [PipeOpPredClassifSurvDiscTime] transforms the resulting [PredictionClassif][mlr3::PredictionClassif] to [PredictionSurv].
#' 4. Optionally: [PipeOpModelMatrix][mlr3pipelines::PipeOpModelMatrix] is used to transform the formula of the task
#' before fitting the learner.
#'
#' @examplesIf mlr3misc::require_namespaces(c("mlr3pipelines", "mlr3learners"), quietly = TRUE)
#' \dontrun{
#'   library(mlr3)
#'   library(mlr3learners)
#'   library(mlr3pipelines)
#'
#'   task = tsk("lung")
#'   part = partition(task)
#'
#'   grlrn = ppl(
#'     "survtoclassif_disctime",
#'     learner = lrn("classif.log_reg"),
#'     cut = 4, # 4 equidistant time intervals
#'     graph_learner = TRUE
#'   )
#'   grlrn$train(task, row_ids = part$train)
#'   grlrn$predict(task, row_ids = part$test)
#' }
#' @export
pipeline_survtoclassif_disctime = function(learner, cut = NULL, max_time = NULL,
                                  rhs = NULL, graph_learner = FALSE) {
  assert_learner(learner, task_type = "classif")
  assert_true("prob" %in% learner$predict_types)

  gr = mlr3pipelines::Graph$new()
  gr$add_pipeop(mlr3pipelines::po("trafotask_survclassif_disctime", cut = cut, max_time = max_time))
  gr$add_pipeop(mlr3pipelines::po("learner", learner, predict_type = "prob"))
  gr$add_pipeop(mlr3pipelines::po("nop"))
  gr$add_pipeop(mlr3pipelines::po("trafopred_classifsurv_disctime"))

  gr$add_edge(src_id = "trafotask_survclassif_disctime", dst_id = learner$id, src_channel = "output", dst_channel = "input")
  gr$add_edge(src_id = "trafotask_survclassif_disctime", dst_id = "nop", src_channel = "transformed_data", dst_channel = "input")
  gr$add_edge(src_id = learner$id, dst_id = "trafopred_classifsurv_disctime", src_channel = "output", dst_channel = "input")
  gr$add_edge(src_id = "nop", dst_id = "trafopred_classifsurv_disctime", src_channel = "output", dst_channel = "transformed_data")

  if (!is.null(rhs)) {
    gr$edges = gr$edges[-1, ]
    gr$add_pipeop(mlr3pipelines::po("modelmatrix", formula = formulate(rhs = rhs, quote = "left")))
    gr$add_edge(src_id = "trafotask_survclassif_disctime", dst_id = "modelmatrix", src_channel = "output")
    gr$add_edge(src_id = "modelmatrix", dst_id = learner$id, src_channel = "output", dst_channel = "input")
  }

  if (graph_learner) {
    gr = mlr3pipelines::GraphLearner$new(gr)
  }

  gr
}

#' @template pipeline
#' @templateVar pipeop [PipeOpTaskSurvClassifIPCW] and [PipeOpPredClassifSurvIPCW]
#' @templateVar id survtoclassif_IPCW
#' @templateVar title Survival to Classification Reduction using IPCW
#' @section Dictionary:
#' Additional alias id for pipeline construction:
#' ```
#' ppl("survtoclassif_vock")
#' ```
#'
#' @param learner [LearnerClassif][mlr3::LearnerClassif]\cr
#' Classification learner to fit the transformed [TaskClassif][mlr3::TaskClassif].
#' @param tau (`numeric()`)\cr
#' Predefined time point for IPCW. Observations with time larger than \eqn{\tau} are censored.
#' Must be less or equal to the maximum event time.
#' @param eps (`numeric()`)\cr
#' Small value to replace \eqn{G(t) = 0} censoring probabilities to prevent infinite
#' weights (a warning is triggered if this happens).
#' @param graph_learner (`logical(1)`)\cr
#' If `TRUE` returns wraps the [Graph][mlr3pipelines::Graph] as a
#' [GraphLearner][mlr3pipelines::GraphLearner] otherwise (default) returns as a `Graph`.
#'
#' @details
#' The pipeline consists of the following steps:
#'
#' 1. [PipeOpTaskSurvClassifIPCW] Converts [TaskSurv] to a [TaskClassif][mlr3::TaskClassif].
#' 2. A [LearnerClassif] is fit and predicted on the new `TaskClassif`.
#' 3. [PipeOpPredClassifSurvIPCW] transforms the resulting [PredictionClassif][mlr3::PredictionClassif]
#' to [PredictionSurv].
#'
#' @examplesIf mlr3misc::require_namespaces(c("mlr3pipelines", "mlr3learners"), quietly = TRUE)
#' \dontrun{
#'   library(mlr3)
#'   library(mlr3learners)
#'   library(mlr3pipelines)
#'
#'   task = tsk("lung")
#'   part = partition(task)
#'
#'   grlrn = ppl(
#'     "survtoclassif_IPCW",
#'     learner = lrn("classif.rpart"),
#'     tau = 500, # Observations after 500 days are censored
#'     graph_learner = TRUE
#'   )
#'   grlrn$train(task, row_ids = part$train)
#'   pred = grlrn$predict(task, row_ids = part$test)
#'   pred # crank and distr at the cutoff time point included
#'
#'   # score predictions
#'   pred$score() # C-index
#'   pred$score(msr("surv.brier", times = 500, integrated = FALSE)) # Brier score at tau
#' }
#' @export
pipeline_survtoclassif_IPCW = function(learner, tau = NULL, eps = 1e-3, graph_learner = FALSE) {
  assert_learner(learner, task_type = "classif")
  assert_true("prob" %in% learner$predict_types)

  gr = mlr3pipelines::Graph$new()
  gr$add_pipeop(mlr3pipelines::po("trafotask_survclassif_IPCW", tau = tau, eps = eps))
  gr$add_pipeop(mlr3pipelines::po("learner", learner, predict_type = "prob"))
  gr$add_pipeop(mlr3pipelines::po("trafopred_classifsurv_IPCW"))
  gr$add_pipeop(mlr3pipelines::po("nop"))

  gr$add_edge(src_id = "trafotask_survclassif_IPCW", dst_id = learner$id, src_channel = "output", dst_channel = "input")
  gr$add_edge(src_id = learner$id, dst_id = "trafopred_classifsurv_IPCW", src_channel = "output", dst_channel = "input")
  gr$add_edge(src_id = "trafotask_survclassif_IPCW", dst_id = "nop", src_channel = "data", dst_channel = "input")
  gr$add_edge(src_id = "nop", dst_id = "trafopred_classifsurv_IPCW", src_channel = "output", dst_channel = "data")

  if (graph_learner) {
    gr = mlr3pipelines::GraphLearner$new(gr)
  }

  gr
}

register_graph("survaverager", pipeline_survaverager)
register_graph("survbagging", pipeline_survbagging)
register_graph("crankcompositor", pipeline_crankcompositor)
register_graph("distrcompositor", pipeline_distrcompositor)
register_graph("responsecompositor", pipeline_responsecompositor)
register_graph("probregr", pipeline_probregr)
register_graph("survtoregr", pipeline_survtoregr)
register_graph("survtoclassif_disctime", pipeline_survtoclassif_disctime)
register_graph("survtoclassif_IPCW", pipeline_survtoclassif_IPCW)
register_graph("survtoclassif_vock", pipeline_survtoclassif_IPCW) # alias
