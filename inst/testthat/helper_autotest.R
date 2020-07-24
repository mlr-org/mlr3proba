generate_tasks.LearnerDens = function(learner, N = 30L, ...) { # nolint
  target = rnorm(N)
  data = cbind(data.table::data.table(target = target), generate_data(learner, N))
  task = TaskDens$new("proto", mlr3::as_data_backend(data), target = "target")

  tasks = generate_generic_tasks(learner, task)

  # generate sanity task
  with_seed(100, {
    data = data.table::data.table(x = c(rnorm(100, 0, 1), rnorm(100, 10, 1)), y = rnorm(200))
    data$unimportant = runif(nrow(data))
  })
  task = mlr3misc::set_names(list(TaskDens$new("sanity",
                                               mlr3::as_data_backend(data), target = "y")),
                             "sanity")
  tasks = c(tasks, task)
}
registerS3method("generate_tasks", "LearnerDens", generate_tasks.LearnerDens)

sanity_check.PredictionDens = function(prediction, ...) { # nolint
  prediction$score() >= -50 & prediction$score() <= 50
}
registerS3method("sanity_check", "PredictionDens", sanity_check.PredictionDens)

generate_tasks.LearnerSurv = function(learner, N = 20L, ...) { # nolint

  real_time = 1 + rexp(N, rate = 2) * 20
  cens_time = 1 + rexp(N, rate = 3) * 20
  status = ifelse(real_time <= cens_time, 1L, 0L)
  obs_time = ifelse(real_time <= cens_time, real_time, cens_time)

  data = cbind(data.table::data.table(time = obs_time, event = status), generate_data(learner, N))
  task = TaskSurv$new("proto", mlr3::as_data_backend(data), time = "time", event = "event")
  tasks = generate_generic_tasks(learner, task)

  # generate sanity task
  set.seed(100)
  data = data.table::data.table(time = obs_time, event = status, x1 = real_time +
                                  rnorm(N, sd = 0.1))
  data$unimportant = runif(nrow(data), 0, 20)
  task = mlr3misc::set_names(list(TaskSurv$new("sanity", mlr3::as_data_backend(data),
                                               time = "time", event = "event")), "sanity")
  tasks = c(tasks, task)
}
registerS3method("generate_tasks", "LearnerSurv", generate_tasks.LearnerSurv)

sanity_check.PredictionSurv = function(prediction, ...) { # nolint
  prediction$score() >= 0
}
registerS3method("sanity_check", "PredictionSurv", sanity_check.PredictionSurv)
