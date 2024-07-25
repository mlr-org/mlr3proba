keys = as.data.table(mlr_tasks)[task_type == "surv"][["key"]]

tasks = lapply(keys, function(key) {
  tsk(key)
})
