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
#' @importFrom stats model.matrix model.frame sd predict density median quantile setNames
#' @importFrom survival Surv
#' @importFrom utils getFromNamespace
#' @importFrom mlr3pipelines po as_graph %>>% pipeline_greplicate gunion Graph ppl
"_PACKAGE"
# nolint end

# to silence RCMD check
utils::globalVariables(c(
  "ShortName", "ClassName", "missing", "task", "value", "variable", "y"
))

.onLoad = function(libname, pkgname) {
  register_mlr3()
  if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
    register_mlr3pipelines()
  }

  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(), action = "append")
  setHook(packageEvent("mlr3pipelines", "onLoad"), function(...) register_mlr3pipelines(),
    action = "append")
}

.onUnload = function(libpath) {
  event = packageEvent("mlr3", "onLoad")
  hooks = getHook(event)
  pkgname = map_chr(hooks[-1L], function(x) environment(x)$pkgname)
  setHook(event, hooks[pkgname != "mlr3proba"], action = "replace")

  event = packageEvent("mlr3pipelines", "onLoad")
  hooks = getHook(event)
  pkgname = map_chr(hooks[-1L], function(x) environment(x)$pkgname)
  setHook(event, hooks[pkgname != "mlr3proba"], action = "replace")

  # unregister
  unregister_reflections()
  walk(names(mlr3proba_learners), function(nm) mlr_learners$remove(nm))
  walk(names(mlr3proba_tasks), function(nm) mlr_tasks$remove(nm))
  walk(names(mlr3proba_measures), function(nm) mlr_measures$remove(nm))
  walk(names(mlr3proba_task_gens), function(nm) mlr_task_generators$remove(nm))
  walk(names(mlr3proba_pipeops), function(nm) mlr3pipelines::mlr_pipeops$remove(nm))
  # manually remove breslow pipeop
  mlr3pipelines::mlr_pipeops$remove("breslowcompose")
  walk(names(mlr3proba_graphs), function(nm) mlr3pipelines::mlr_graphs$remove(nm))

  library.dynam.unload("mlr3proba", libpath)
}

unregister_reflections = function() {
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")

  # task
  package = NULL # silence data.table notes
  x$task_types = x$task_types[package != "mlr3proba"]
  x$task_col_roles$surv = NULL
  x$task_col_roles$dens = NULL
  x$task_col_roles$classif = setdiff(x$task_col_roles$classif, "original_ids")
  x$task_properties$surv = NULL
  x$task_properties$dens = NULL

  # learner
  x$learner_properties$surv = NULL
  x$learner_properties$dens = NULL
  x$learner_predict_types$surv = NULL
  x$learner_predict_types$dens = NULL

  # measure
  x$measure_properties$surv = NULL
  x$measure_properties$dens = NULL
  x$default_measures$surv = NULL
  x$default_measures$dens = NULL
}

leanify_package()
