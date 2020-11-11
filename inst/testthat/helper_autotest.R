generate_tasks.LearnerDens = function(learner, N = 30L, ...) { # nolint
  task = TaskDens$new("proto", rnorm(N))

  tasks = list()
  tasks$feat_single_integer = TaskDens$new("feat_single_integer",
                                           data.frame(integer = rbinom(N, 10, 0.5)))
  tasks$feat_single_numeric = TaskDens$new("feat_single_numeric", data.frame(numeric = rnorm(N)))
  tasks$weights = TaskDens$new("feat_single_numeric", data.frame(x = rnorm(N), weights = runif(N)))

  # generate sanity task
  data = with_seed(100, rnorm(1000, 10, 1))
  tasks$sanity = TaskDens$new("sanity", data)
  tasks$sanity_reordered = tasks$sanity$clone(deep = TRUE)

  tasks
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
  data = with_seed(100, {
    data = data.table::data.table(time = obs_time, event = status, x1 = real_time + rnorm(N, sd = 0.1),
      unimportant = runif(N, 0, 20))
  })
  tasks$sanity = TaskSurv$new("sanity", mlr3::as_data_backend(data), time = "time", event = "event")
  tasks$sanity_reordered = tasks$sanity$clone(deep = TRUE)

  tasks
}
registerS3method("generate_tasks", "LearnerSurv", generate_tasks.LearnerSurv)

sanity_check.PredictionSurv = function(prediction, ...) { # nolint
  prediction$score() >= 0.5
}
registerS3method("sanity_check", "PredictionSurv", sanity_check.PredictionSurv)
