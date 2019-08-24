lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

expect_task_density = function(task) {
  expect_is(task, "TaskDensity")
  expect_task(task)
  expect_task_supervised(task)
  expect_is(task$truth(), "numeric")

  f = task$formula()
  expect_formula(f)
  expect_set_equal(extract_vars(f)$lhs, task$target_names)
}

generate_tasks.LearnerDensity = function(learner, N = 30L) {
  target = rnorm(N)
  data = cbind(data.table::data.table(target = target), generate_data(learner, N))
  task = TaskDensity$new("proto", mlr3::as_data_backend(data), target = "target")

  tasks = generate_generic_tasks(learner, task)

  # generate sanity task
  with_seed(100, {
    data = data.table::data.table(x = c(rnorm(100, 0, 1), rnorm(100, 10, 1)), y = c(rep(0, 100), rep(1, 100)))
    data$unimportant = runif(nrow(data))
  })
  task = mlr3misc::set_names(list(TaskDensity$new("sanity", mlr3::as_data_backend(data), target = "y")), "sanity")
  tasks = c(tasks, task)
}

registerS3method("generate_tasks", "LearnerDensity", generate_tasks.LearnerDensity)


sanity_check.PredictionDensity = function(prediction) {
  prediction$score() >= 0.6
}
registerS3method("sanity_check", "PredictionDensity", sanity_check.PredictionDensity)

expect_prediction_density = function(p) {
  expect_prediction(p)
  expect_is(p, "PredictionDensity")
}

