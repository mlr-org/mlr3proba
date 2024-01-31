#' @importFrom Rcpp sourceCpp
#' @useDynLib mlr3proba
NULL

#' @title Cpp functions
#' @name cpp
#' @description Exported internal cpp functions for developers
#' @keywords internal
NULL

#' @name .c_weight_survival_score
#' @rdname cpp
#' @export
NULL

#' @name .c_get_unique_times
#' @rdname cpp
#' @export
NULL

# nolint start
#' @import checkmate
#' @import data.table
#' @import distr6
#' @import ggplot2
#' @import mlr3
#' @import mlr3misc
#' @import paradox
#' @importFrom R6 R6Class
#' @importFrom utils data head tail
#' @importFrom stats reformulate model.matrix model.frame sd predict complete.cases density
#' @importFrom survival Surv
#' @importFrom mlr3viz fortify
"_PACKAGE"
# nolint end

# to silence RCMD check
utils::globalVariables(c(
  "ShortName", "ClassName", "missing", "task",
  "value", "variable", "y"
))

mlr3proba_learners = new.env()
mlr3proba_tasks = new.env()
mlr3proba_measures = new.env()

register_learner = function(name, constructor) {
  assert_class(constructor, "R6ClassGenerator")
  if (name %in% names(mlr3proba_learners)) {
    stopf("learner %s registered twice", name)
  }
  mlr3proba_learners[[name]] = constructor # fn
}

register_task = function(name, constructor) {
  if (name %in% names(mlr3proba_tasks)) stopf("task %s registered twice", name)
  mlr3proba_tasks[[name]] = constructor
}

register_measure = function(name, constructor) {
  if (name %in% names(mlr3proba_measures)) stopf("measure %s registered twice", name)
  mlr3proba_measures[[name]] = constructor
}

register_mlr3 = function() {
  # reflections
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")

  # task
  x$task_types = x$task_types[!c("surv", "dens")]
  x$task_types = setkeyv(rbind(x$task_types, rowwise_table(
    ~type,  ~package,     ~task,      ~learner,      ~prediction,       ~prediction_data,     ~measure,
    "surv", "mlr3proba",  "TaskSurv", "LearnerSurv", "PredictionSurv",  "PredictionDataSurv", "MeasureSurv",
    "dens", "mlr3proba",  "TaskDens", "LearnerDens", "PredictionDens",  "PredictionDataDens", "MeasureDens"
  )), "type")

  x$task_col_roles$surv = x$task_col_roles$regr
  x$task_col_roles$dens = c("feature", "target", "label", "order", "group", "weight", "stratum")
  x$task_properties$surv = x$task_properties$regr
  x$task_properties$dens = x$task_properties$regr

  # learner
  x$learner_properties$surv = x$learner_properties$regr
  x$learner_properties$dens = x$learner_properties$regr
  x$learner_predict_types$surv = list(
    crank = c("crank", "lp", "distr", "response"),
    distr = c("crank", "lp", "distr", "response"),
    lp = c("crank", "lp", "distr", "response"),
    response = c("crank", "lp", "distr", "response"))
  x$learner_predict_types$dens = list(
    pdf = c("pdf", "cdf", "distr"),
    cdf = c("pdf", "cdf", "distr"),
    distr = c("pdf", "cdf", "distr"))

  # measure
  x$measure_properties$surv = x$measure_properties$regr
  x$measure_properties$dens = x$measure_properties$regr
  x$default_measures$surv = "surv.cindex"
  x$default_measures$dens = "dens.logloss"

  # tasks
  mlr_tasks = utils::getFromNamespace("mlr_tasks", ns = "mlr3")
  iwalk(as.list(mlr3proba_tasks), function(obj, name) mlr_tasks$add(name, obj)) # nolint

  # generators
  x = utils::getFromNamespace("mlr_task_generators", ns = "mlr3")
  x$add("simdens", TaskGeneratorSimdens)
  x$add("simsurv", TaskGeneratorSimsurv)

  # learners
  mlr_learners = utils::getFromNamespace("mlr_learners", ns = "mlr3")
  iwalk(as.list(mlr3proba_learners), function(obj, name) mlr_learners$add(name, obj)) # nolint

  # measures
  mlr_measures = utils::getFromNamespace("mlr_measures", ns = "mlr3")
  iwalk(as.list(mlr3proba_measures), function(obj, name) mlr_measures$add(name, obj)) # nolint
}

register_mlr3pipelines = function() {
  mlr3pipelines::add_class_hierarchy_cache(c("PredictionSurv", "Prediction"))

  x = utils::getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")

  x$add("distrcompose", PipeOpDistrCompositor)
  x$add("crankcompose", PipeOpCrankCompositor)
  x$add("breslowcompose", PipeOpBreslow, list(R6Class("Learner",
    public = list(id = "breslowcompose", task_type = "surv", predict_types = "lp",
      packages = c("mlr3", "mlr3proba"), param_set = ps()))$new()))

  x$add("trafotask_regrsurv", PipeOpTaskRegrSurv)
  x$add("trafotask_survregr", PipeOpTaskSurvRegr)
  x$add("trafopred_regrsurv", PipeOpPredRegrSurv)
  x$add("trafopred_survregr", PipeOpPredSurvRegr)

  x$add("compose_distr", PipeOpDistrCompositor)
  x$add("compose_crank", PipeOpCrankCompositor)
  x$add("compose_probregr", PipeOpProbregr)

  x$add("survavg", PipeOpSurvAvg)

  x = utils::getFromNamespace("mlr_graphs", ns = "mlr3pipelines")
  x$add("distrcompositor", pipeline_distrcompositor)
  x$add("crankcompositor", pipeline_crankcompositor)
  x$add("probregr", pipeline_probregr)
  x$add("survaverager", pipeline_survaverager)
  x$add("survbagging", pipeline_survbagging)
  x$add("survtoregr", pipeline_survtoregr)
}

.onLoad = function(libname, pkgname) { # nolint
  register_mlr3()
  if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
    register_mlr3pipelines()
  }

  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(), action = "append")
  setHook(packageEvent("mlr3pipelines", "onLoad"), function(...) register_mlr3pipelines(),
    action = "append")
}

.onUnload = function(libpath) { # nolint
  event = packageEvent("mlr3", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks[-1], function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "mlr3proba"], action = "replace")

  event = packageEvent("mlr3pipelines", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks[-1], function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "mlr3proba"], action = "replace")

  # unregister
  walk(names(mlr3proba_learners), function(nm) mlr_learners$remove(nm))
  walk(names(mlr3proba_tasks), function(nm) mlr_tasks$remove(nm))
  walk(names(mlr3proba_measures), function(nm) mlr_measures$remove(nm))

  library.dynam.unload("mlr3proba", libpath)
}

leanify_package()
