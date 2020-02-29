load_task_precip = function(id = "precip") {
  b = as_data_backend(data.table::data.table(precip = load_dataset("precip", "datasets", keep_rownames = TRUE)))
  b$hash = "_mlr3_tasks_precip_"
  TaskDens$new(id, b, target = "precip")
}
