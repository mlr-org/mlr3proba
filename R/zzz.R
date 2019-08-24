register_mlr3 = function() {

  # let mlr3 know about density, probs
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  x$task_types = data.table::setkeyv(rbind(x$task_types, rowwise_table(
    ~type,  ~package,       ~task,      ~learner,      ~prediction,      ~measure,
    "density", "mlr3pro", "TaskDensity", "LearnerDensity", "PredictionDensity", "MeasureDensity"
  )), "type")
  x$task_col_roles$density = x$task_col_roles$regr
  x$task_properties$density = c("weights", "groups")
  x$learner_properties$density = x$learner_properties$regr
  x$learner_predict_types$density$prob = "prob"
  x$default_measures$density = "classif.logloss"

  # tasks
   x = utils::getFromNamespace("mlr_tasks", ns = "mlr3")
   x$add("precip", load_task_precip)
  # x$add("lung", load_lung)
  # x$add("unemployment", load_unemployment)

  # generators
   x = utils::getFromNamespace("mlr_task_generators", ns = "mlr3")
   x$add("friedman1dens", TaskGeneratorFriedman1Dens)

  # learners
   x = utils::getFromNamespace("mlr_learners", ns = "mlr3")
   x$add("density.kde", LearnerDensityKDE)
  # x$add("surv.glmnet", LearnerSurvGlmnet)
  # x$add("surv.rpart", LearnerSurvRpart)
  # x$add("surv.ranger", LearnerSurvRanger)
  # x$add("surv.featureless", LearnerSurvFeatureless)

  # measures
   x = utils::getFromNamespace("mlr_measures", ns = "mlr3")
   x$add("classif.logloss", MeasureClassifLogloss)
  # x$add("surv.unos_c", MeasureSurvUnosC)
}

.onLoad = function(libname, pkgname) {
  library(checkmate)
  # nocov start
  register_mlr3()
  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(), action = "append")
} # nocov end

# .onUnload = function(libpath) {
#   # nocov start
#   event = packageEvent("mlr3", "onLoad")
#   hooks = getHook(event)
#   pkgname = vapply(hooks, function(x) environment(x)$pkgname, NA_character_)
#   setHook(event, hooks[pkgname != "mlr3pro"], action = "replace")
# } # nocov end
