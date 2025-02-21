# These elements need to be at the top of the Collate: order!

mlr3proba_learners = new.env()
mlr3proba_tasks = new.env()
mlr3proba_measures = new.env()
mlr3proba_task_gens = new.env()
mlr3proba_pipeops = new.env()
mlr3proba_graphs = new.env()

register_learner = function(name, constructor) {
  assert_class(constructor, "R6ClassGenerator")
  if (name %in% names(mlr3proba_learners)) stopf("learner %s registered twice", name)
  mlr3proba_learners[[name]] = constructor
}

register_task = function(name, constructor) {
  if (name %in% names(mlr3proba_tasks)) stopf("task %s registered twice", name)
  mlr3proba_tasks[[name]] = constructor
}

register_measure = function(name, constructor) {
  if (name %in% names(mlr3proba_measures)) stopf("measure %s registered twice", name)
  mlr3proba_measures[[name]] = constructor
}

register_task_generator = function(name, constructor) {
  if (name %in% names(mlr3proba_task_gens)) stopf("task generator %s registered twice", name)
  mlr3proba_task_gens[[name]] = constructor
}

register_pipeop = function(name, constructor) {
  if (name %in% names(mlr3proba_pipeops)) stopf("pipeop %s registered twice", name)
  mlr3proba_pipeops[[name]] = constructor
}

register_graph = function(name, constructor) {
  if (name %in% names(mlr3proba_graphs)) stopf("graph %s registered twice", name)
  mlr3proba_graphs[[name]] = constructor
}

register_reflections = function() {
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")

  # task types
  x$task_types = x$task_types[!c("surv", "dens", "cmprsk")]
  x$task_types = setkeyv(rbind(x$task_types, rowwise_table(
    ~type,  ~package,     ~task,      ~learner,      ~prediction,       ~prediction_data,     ~measure,
    "surv", "mlr3proba",  "TaskSurv", "LearnerSurv", "PredictionSurv",  "PredictionDataSurv", "MeasureSurv",
    "dens", "mlr3proba",  "TaskDens", "LearnerDens", "PredictionDens",  "PredictionDataDens", "MeasureDens",
    "cmprsk", "mlr3proba",  "TaskCompRisks", "LearnerCompRisks", "PredictionCompRisks",  "PredictionDataCompRisks", "MeasureCompRisks"
  )), "type")

  # task column roles
  x$task_col_roles$surv = x$task_col_roles$regr
  x$task_col_roles$cmprsk = x$task_col_roles$regr
  x$task_col_roles$dens = c("feature", "target", "label", "order", "group", "weight", "stratum")
  x$task_col_roles$classif = unique(c(x$task_col_roles$classif, "original_ids")) # for discrete time

  # task properties
  x$task_properties$surv = x$task_properties$regr
  x$task_properties$cmprsk = x$task_properties$regr
  x$task_properties$dens = x$task_properties$regr

  # learner properties
  x$learner_properties$surv = x$learner_properties$regr
  x$learner_properties$cmprsk = x$learner_properties$regr
  x$learner_properties$dens = x$learner_properties$regr

  # learner predict types
  x$learner_predict_types$surv = list(
    # for survival we all predict types are possible in theory via transformations
    crank = c("crank", "lp", "distr", "response"),
    distr = c("crank", "lp", "distr", "response"),
    lp = c("crank", "lp", "distr", "response"),
    response = c("crank", "lp", "distr", "response")
  )
  x$learner_predict_types$cmprsk = list(cif = "cif")
  x$learner_predict_types$dens = list(
    pdf = c("pdf", "cdf", "distr"),
    cdf = c("pdf", "cdf", "distr"),
    distr = c("pdf", "cdf", "distr")
  )

  # measure
  x$measure_properties$surv = x$measure_properties$regr
  x$measure_properties$dens = x$measure_properties$regr
  x$default_measures$surv = "surv.cindex"
  x$default_measures$dens = "dens.logloss"
}

register_mlr3 = function() {
  # reflections
  register_reflections()

  # tasks
  mlr_tasks = utils::getFromNamespace("mlr_tasks", ns = "mlr3")
  iwalk(as.list(mlr3proba_tasks), function(obj, name) mlr_tasks$add(name, obj)) # nolint

  # task generators
  mlr_task_gens = utils::getFromNamespace("mlr_task_generators", ns = "mlr3")
  iwalk(as.list(mlr3proba_task_gens), function(obj, name) mlr_task_gens$add(name, obj)) # nolint

  # learners
  mlr_learners = utils::getFromNamespace("mlr_learners", ns = "mlr3")
  iwalk(as.list(mlr3proba_learners), function(obj, name) mlr_learners$add(name, obj)) # nolint

  # measures
  mlr_measures = utils::getFromNamespace("mlr_measures", ns = "mlr3")
  iwalk(as.list(mlr3proba_measures), function(obj, name) mlr_measures$add(name, obj)) # nolint
}

register_mlr3pipelines = function() {
  mlr3pipelines::add_class_hierarchy_cache(c("PredictionSurv", "Prediction"))

  # pipeops
  mlr_pipeops = utils::getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")
  iwalk(as.list(mlr3proba_pipeops), function(obj, name) mlr_pipeops$add(name, obj)) # nolint

  # Breslow PipeOp needs one more argument so we do it manually
  mlr_pipeops$add("breslowcompose", PipeOpBreslow,
                  list(R6Class("Learner", public = list(
                    id = "breslowcompose", task_type = "surv", predict_types = "lp",
                    packages = c("mlr3", "mlr3proba"), param_set = ps()))$new()))

  # graphs
  mlr_graphs = utils::getFromNamespace("mlr_graphs", ns = "mlr3pipelines")
  iwalk(as.list(mlr3proba_graphs), function(obj, name) mlr_graphs$add(name, obj)) # nolint
}
