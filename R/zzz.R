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
#' @importFrom utils getFromNamespace
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
  walk(names(mlr3proba_task_gens), function(nm) mlr_task_generators$remove(nm))
  walk(names(mlr3proba_pipeops), function(nm) mlr3pipelines::mlr_pipeops$remove(nm))
  walk(names(mlr3proba_graphs), function(nm) mlr3pipelines::mlr_graphs$remove(nm))

  library.dynam.unload("mlr3proba", libpath)
}

leanify_package()
