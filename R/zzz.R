#' @import checkmate
#' @import data.table
#' @import distr6
#' @import mlr3
#' @import mlr3misc
#' @import mlr3pipelines
#' @import paradox
#' @importFrom R6 R6Class
#' @importFrom utils data head tail
#' @importFrom stats reformulate model.matrix model.frame sd
#' @importFrom survival Surv
"_PACKAGE"

register_mlr3 = function() {

  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")

  if (!grepl("surv", x$task_types[,"type"])) {
    x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
    x$task_types = setkeyv(rbind(x$task_types, rowwise_table(
      ~type,     ~package,    ~task,         ~learner,         ~prediction,         ~measure,
      "surv",    "mlr3proba", "TaskSurv",    "LearnerSurv",    "PredictionSurv",    "MeasureSurv"
    )), "type")
    x$task_col_roles$surv = c("feature", "target", "label", "order", "group", "weight", "stratum")
    x$task_properties$surv = c("weights", "groups")
    x$learner_properties$surv = x$learner_properties$regr
    x$measure_properties$surv = x$measure_properties$regr
    x$learner_predict_types$surv = list(crank = c("crank","lp","distr","response"),
                                        distr = c("crank","lp","distr","response"),
                                        lp = c("crank","lp","distr","response"),
                                        response = c("crank","lp","distr","response"))
    x$default_measures$surv = "surv.harrellC"
  }

  if (!grepl("dens", x$task_types[,"type"])) {
     x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
     x$task_types = setkeyv(rbind(x$task_types, rowwise_table(
        ~type,     ~package,    ~task,         ~learner,         ~prediction,         ~measure,
        "dens",    "mlr3proba", "TaskDens",    "LearnerDens",    "PredictionDens",    "MeasureDens"
     )), "type")
     x$task_col_roles$dens = c("feature", "target", "label", "order", "group", "weight", "stratum")
     x$task_properties$dens = c("weights", "groups")
     x$learner_properties$dens = x$learner_properties$regr
     x$measure_properties$dens = x$measure_properties$regr
     x$learner_predict_types$dens = list(pdf = c("pdf","cdf"),
                                         cdf = c("pdf","cdf"))
     x$default_measures$dens = "dens.logloss"
  }

  # tasks
   x = utils::getFromNamespace("mlr_tasks", ns = "mlr3")
   x$add("precip", load_precip)
   x$add("faithful", load_faithful)
   x$add("rats", load_rats)
   x$add("lung", load_lung)
   x$add("unemployment", load_unemployment)

  # generators
   x = utils::getFromNamespace("mlr_task_generators", ns = "mlr3")
   x$add("simdens", TaskGeneratorSimdens)
   x$add("simsurv", TaskGeneratorSimsurv)

  # learners
   x = utils::getFromNamespace("mlr_learners", ns = "mlr3")

   x$add("dens.hist", LearnerDensHistogram)
   x$add("dens.kde", LearnerDensKDE)
   x$add("dens.plug", LearnerDensPlugin)
   x$add("dens.kde", LearnerDensHistogram)
   x$add("dens.locfit", LearnerDensLocfit)
   x$add("dens.kde", LearnerDensKDE)
   x$add("dens.nonpar", LearnerDensNonparametric)
   x$add("dens.mixed", LearnerDensMixed)
   x$add("dens.logspline", LearnerDensLogspline)
   x$add("dens.pen", LearnerDensPenalized)
   x$add("dens.kdeKD", LearnerDensKDEkd)
   x$add("dens.kdeKS", LearnerDensKDEks)
   x$add("dens.spline", LearnerDensSpline)

   x$add("surv.coxph", LearnerSurvCoxPH)
   x$add("surv.kaplan", LearnerSurvKaplan)
   x$add("surv.nelson", LearnerSurvNelson)
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
   x$add("surv.gbm", LearnerSurvGBM)
   x$add("surv.obliqueRSF", LearnerSurvObliqueRSF)

  # measures
   x = utils::getFromNamespace("mlr_measures", ns = "mlr3")

   x$add("dens.logloss", MeasureDensLogloss)

   # x$add("regr.logloss", MeasureRegrLogloss)

   x$add("surv.graf", MeasureSurvGraf)
   x$add("surv.grafSE", MeasureSurvGrafSE)
   x$add("surv.logloss", MeasureSurvLogloss)
   x$add("surv.loglossSE", MeasureSurvLoglossSE)
   x$add("surv.intlogloss", MeasureSurvIntLogloss)
   x$add("surv.intloglossSE", MeasureSurvIntLoglossSE)

   x$add("surv.unoC", MeasureSurvUnoC)
   x$add("surv.harrellC", MeasureSurvHarrellC)
   x$add("surv.gonenC", MeasureSurvGonenC)
   x$add("surv.beggC", MeasureSurvBeggC)

   x$add("surv.nagelkR2", MeasureSurvNagelkR2)
   x$add("surv.oquigleyR2", MeasureSurvOQuigleyR2)
   x$add("surv.xuR2", MeasureSurvXuR2)

   x$add("surv.chamblessAUC", MeasureSurvChamblessAUC)
   x$add("surv.hungAUC", MeasureSurvHungAUC)
   x$add("surv.unoAUC", MeasureSurvUnoAUC)
   x$add("surv.songAUC", MeasureSurvSongAUC)

   x$add("surv.unoTPR", MeasureSurvUnoTPR)
   x$add("surv.songTPR", MeasureSurvSongTPR)

   x$add("surv.unoTNR", MeasureSurvUnoTNR)
   x$add("surv.songTNR", MeasureSurvSongTNR)

   x$add("surv.rmse", MeasureSurvRMSE)
   x$add("surv.rmseSE", MeasureSurvRMSESE)
   x$add("surv.mse", MeasureSurvMSE)
   x$add("surv.mseSE", MeasureSurvMSESE)
   x$add("surv.mae", MeasureSurvMAE)
   x$add("surv.maeSE", MeasureSurvMAESE)
}
register_mlr3pipelines = function(){
   x = utils::getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")
   x$add("distrcompose", PipeOpDistrCompositor)
   x$add("crankcompose", PipeOpCrankCompositor)
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
