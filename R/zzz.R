#' @import data.table
#' @import checkmate
#' @import paradox
#' @import mlr3
#' @import mlr3misc
#' @import mlr3pipelines
#' @importFrom R6 R6Class
#' @importFrom utils data head tail
#' @importFrom stats reformulate model.matrix model.frame
#' @importFrom survival Surv
#' @importFrom BBmisc suppressAll
"_PACKAGE"

register_mlr3 = function() {

  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")

  # add density task
  if (!grepl("density", x$task_types[,"type"])) {
     x$task_types = data.table::setkeyv(rbind(x$task_types, rowwise_table(
       ~type,  ~package,       ~task,      ~learner,      ~prediction,      ~measure,
       "density", "mlr3proba", "TaskDensity", "LearnerDensity", "PredictionDensity", "MeasureDensity"
     )), "type")
     x$task_col_roles$density = x$task_col_roles$regr
     x$task_properties$density = c("weights", "groups")
     x$learner_properties$density = x$learner_properties$regr
     x$learner_predict_types$density$prob = "prob"
     x$default_measures$density = "density.logloss"
  }

  # add distr and interval to regression task
  x$learner_predict_types$regr$distr = "distr"
  x$learner_predict_types$regr$interval = "interval"

  # add survival task
  if (!grepl("surv", x$task_types[,"type"])) {
     x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
     x$task_types = setkeyv(rbind(x$task_types, rowwise_table(
       ~type,  ~package,       ~task,      ~learner,      ~prediction,      ~measure,
       "surv", "mlr3proba", "TaskSurv", "LearnerSurv", "PredictionSurv", "MeasureSurv"
     )), "type")
     x$task_col_roles$surv = c("feature", "target", "label", "order", "groups", "weights")
     x$task_properties$surv = c("weights", "groups")
     x$learner_properties$surv = x$learner_properties$regr
     x$measure_properties$surv = x$measure_properties$regr
     x$learner_predict_types$surv = list(crank = c("crank","lp","distr"),
                                         distr = c("crank","lp","distr"),
                                         lp = c("crank","lp","distr"))
     x$default_measures$surv = "surv.harrellsc"
  }

  # tasks
   x = utils::getFromNamespace("mlr_tasks", ns = "mlr3")
   x$add("precip", load_task_precip)
   x$add("rats", load_rats)
   x$add("lung", load_lung)
   x$add("unemployment", load_unemployment)

  # generators
   x = utils::getFromNamespace("mlr_task_generators", ns = "mlr3")
   x$add("friedman1dens", TaskGeneratorFriedman1Dens)
   x$add("simsurv", TaskGeneratorSimsurv)

  # learners
   x = utils::getFromNamespace("mlr_learners", ns = "mlr3")
   x$add("density.kde", LearnerDensityKDE)

   x$add("probreg.gaussian", LearnerProbregGaussian)

   x$add("surv.coxph", LearnerSurvCoxPH)
   x$add("surv.kaplan", LearnerSurvKaplanMeier)
   x$add("surv.nelson", LearnerSurvNelsonAalen)
   x$add("surv.glmnet", LearnerSurvGlmnet)
   x$add("surv.cvglmnet", LearnerSurvCVGlmnet)
   x$add("surv.glmboost", LearnerSurvGlmboost)
   x$add("surv.gamboost", LearnerSurvGamboost)
   x$add("surv.mboost", LearnerSurvMboost)
   x$add("surv.blackboost", LearnerSurvBlackboost)
   x$add("surv.penalized", LearnerSurvPenalized)
   x$add("surv.rpart", LearnerSurvRpart)
   x$add("surv.ranger", LearnerSurvRanger)
   x$add("surv.randomForestSRC", LearnerSurvRandomForestSRC)
   x$add("surv.svm", LearnerSurvSVM)
   x$add("surv.parametric", LearnerSurvParametric)
   x$add("surv.flexible", LearnerSurvFlexible)

  # measures
   x = utils::getFromNamespace("mlr_measures", ns = "mlr3")
   x$add("density.logloss", MeasureDensityLogloss)
   x$add("regr.logloss", MeasureRegrLogloss)
   x$add("surv.brier", MeasureSurvBrier)
   x$add("surv.logloss", MeasureSurvLogloss)
   x$add("surv.logloss_se", MeasureSurvLoglossSE)
   x$add("surv.unosc", MeasureSurvUnosC)
   x$add("surv.harrellsc", MeasureSurvHarrellsC)
}
register_mlr3pipelines = function(){
   x = utils::getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")
   x$add("distrcompose", PipeOpDistrCompositor)
}

.onLoad = function(libname, pkgname) {
   register_mlr3()
   register_mlr3pipelines()
   setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(), action = "append")
   setHook(packageEvent("mlr3pipelines", "onLoad"), function(...) register_mlr3pipelines(), action = "append")
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
}
