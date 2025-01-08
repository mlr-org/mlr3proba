skip("Test manually it works and update as needed")

test_that("unloading leaves no trace", {
  library(mlr3proba)
  library(mlr3pipelines)

  # keep ids from loaded mlr3proba-objects
  proba_lrns = mlr_learners$keys(pattern = "surv|dens")
  proba_msrs = mlr_measures$keys(pattern = "surv|dens|regr.logloss")
  task_dt = as.data.table(mlr_tasks)
  proba_task_keys = task_dt[task_type %in% c("surv", "dens"), list(key)][[1]]
  tgen_dt = as.data.table(mlr_task_generators)
  proba_tgen_keys = tgen_dt[task_type %in% c("surv", "dens"), list(key)][[1]]

  # UPDATE below list when new pipeops are implemented
  proba_pipeops = c(
    # miscellaneous
    "survavg", "compose_probregr",
    # compose prediction types
    "crankcompose", "distrcompose", "responsecompose", "breslowcompose",
    # transform prediction type
    "trafopred_classifsurv_disctime", "trafopred_classifsurv_IPCW",
    # transform task type
    "trafotask_survclassif_disctime", "trafotask_survclassif_IPCW"
  )
  expect_in(proba_pipeops, mlr_pipeops$keys())

  # UPDATE below list when new pipelines are implemented
  proba_graphs = c(
    # miscellaneous
    "survaverager", "survbagging", "probregr",
    # compose prediction types
    "crankcompositor", "distrcompositor", "responsecompositor",
    # transform surv to other tasks
    "survtoclassif_disctime", "survtoclassif_IPCW", "survtoclassif_vock"
  )
  expect_in(proba_graphs, mlr_graphs$keys())

  unloadNamespace("mlr3proba")

  # no mlr3proba learners left
  expect_setequal(intersect(proba_lrns, mlr_learners$keys()), character(0))
  # no mlr3proba measures left
  expect_setequal(intersect(proba_msrs, mlr_measures$keys()), character(0))
  # no mlr3proba tasks left
  expect_setequal(intersect(proba_task_keys, mlr_tasks$keys()), character(0))
  # no mlr3proba task generators left
  expect_setequal(intersect(proba_tgen_keys, mlr_task_generators$keys()), character(0))
  # no mlr3proba pipeops left
  expect_setequal(intersect(proba_pipeops, mlr_pipeops$keys()), character(0))
  # no mlr3proba pipelines/graphs left
  expect_setequal(intersect(proba_graphs, mlr_graphs$keys()), character(0))
})
