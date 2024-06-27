test_that("unloading leaves no trace", {
  library(mlr3proba)
  library(mlr3pipelines)

  mlr3proba_lrns = mlr_learners$keys(pattern = "surv|dens")
  mlr3proba_msrs = mlr_measures$keys(pattern = "surv|dens|regr.logloss")
  task_dt = as.data.table(mlr_tasks)
  mlr3proba_task_keys = task_dt[task_type %in% c("surv", "dens"), list(key)][[1]]
  tgen_dt = as.data.table(mlr_task_generators)
  mlr3proba_tgen_keys = tgen_dt[task_type %in% c("surv", "dens"), list(key)][[1]]
  # `as.data.table(mlr_pipeops)` takes too much time,
  # so have the below list, which needs to be UPDATED when new pipeops are implemented
  mlr3proba_pipeops = c("survavg", "trafopred_classifsurv", "trafopred_survregr",
                        "trafotask_regrsurv", "crankcompose", "compose_probregr",
                        "distrcompose", "trafopred_regrsurv", "trafotask_survregr",
                        "trafotask_survclassif", "breslowcompose")
  expect_true(all(mlr3proba_pipeops %in% mlr_pipeops$keys()))
  # UPDATE the below list when new pipelines are implemented
  mlr3proba_graphs = c("survaverager", "survbagging", "crankcompositor",
                       "distrcompositor", "probregr", "survtoregr",
                       "survtoclassif")
  expect_true(all(mlr3proba_graphs %in% mlr_graphs$keys()))

  unloadNamespace("mlr3proba")

  # no mlr3proba learners left
  expect_true(all(mlr3proba_lrns %nin% mlr_learners$keys()))
  # no mlr3proba measures left
  expect_true(all(mlr3proba_msrs %nin% mlr_measures$keys()))
  # no mlr3proba tasks left
  expect_true(all(mlr3proba_task_keys %nin% mlr_tasks$keys()))
  # no mlr3proba task generators left
  expect_true(all(mlr3proba_tgen_keys %nin% mlr_task_generators$keys()))
  # no mlr3proba pipeops left
  expect_true(all(mlr3proba_pipeops %nin% mlr_pipeops$keys()))
  # no mlr3proba pipelines/graphs left
  expect_true(all(mlr3proba_graphs %nin% mlr_graphs$keys()))
})
