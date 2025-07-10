generate_tasks.LearnerDens = function(learner, N = 30L, ...) { # nolint
  task = mlr3proba::TaskDens$new("proto", rnorm(N))

  tasks = list()
  if ("integer" %in% learner$feature_types) {
    tasks$feat_single_integer = mlr3proba::TaskDens$new("feat_single_integer",
                                             data.frame(integer = rbinom(N, 10, 0.5)))
  }
  if("numeric" %in% learner$feature_types) {
    tasks$feat_single_numeric = mlr3proba::TaskDens$new("feat_single_numeric", data.frame(numeric = rnorm(N)))
    tasks$weights = mlr3proba::TaskDens$new("feat_single_numeric", data.frame(x = rnorm(N), weights = runif(N)))
  }

  # generate sanity task
  data = with_seed(100, rnorm(1000, 10, 1))
  tasks$sanity = mlr3proba::TaskDens$new("sanity", data)

  tasks
}
registerS3method("generate_tasks", "LearnerDens", generate_tasks.LearnerDens)

sanity_check.PredictionDens = function(prediction, ...) { # nolint
  prediction$score() >= -50 & prediction$score() <= 50
}
registerS3method("sanity_check", "PredictionDens", sanity_check.PredictionDens)

generate_tasks.LearnerSurv = function(learner, N = 20L, ...) { # nolint
  real_time = round(1 + rexp(N, rate = 2) * 20, 1)
  cens_time = round(1 + rexp(N, rate = 3) * 20, 1)
  status = ifelse(real_time <= cens_time, 1L, 0L)
  obs_time = ifelse(real_time <= cens_time, real_time, cens_time)

  data = cbind(data.table::data.table(time = obs_time, event = status), generate_data(learner, N))
  task = mlr3proba::TaskSurv$new("proto", mlr3::as_data_backend(data), time = "time", event = "event")
  tasks = generate_generic_tasks(learner, task)

  # generate sanity task
  if (N %% 2 == 1) N = N + 1
  data = with_seed(100, {
    data.table::data.table(
    x = c(rep(0, N/2), rep(1, N/2)),
    unimportant = runif(N),
    time = c(rep(10, N/2), rep(100, N/2)),
    event = rbinom(N, 1, 0.9)
  )})
  tasks$sanity = mlr3proba::TaskSurv$new("sanity", mlr3::as_data_backend(data), time = "time", event = "event")

  tasks
}
registerS3method("generate_tasks", "LearnerSurv", generate_tasks.LearnerSurv)

sanity_check.PredictionSurv = function(prediction, ...) {
  # sanity check discrimination
  prediction$score() >= 0.6
}
registerS3method("sanity_check", "PredictionSurv", sanity_check.PredictionSurv)

generate_tasks.LearnerCompRisks = function(learner, N = 20L, ...) {
  times = stats::rexp(N, rate = 0.2) # exp distr for event times
  # two competing events (0 => censored (30%), 1,2 => events (35% each))
  event = sample(0:2, size = N, replace = TRUE, prob = c(0.3, 0.35, 0.35))

  data = cbind(data.table::data.table(time = times, event = event), generate_data(learner, N))
  task = mlr3proba::TaskCompRisks$new(id = "proto", backend = mlr3::as_data_backend(data))
  tasks = generate_generic_tasks(learner, task)

  # Generate sanity task
  # Ensure N is even
  if (N %% 2 == 1L) N = N + 1L
  N_half = N / 2L

  # Generate unique times
  times_group0 = seq(1, N_half)
  times_group1 = seq(max(times_group0) + 1, max(times_group0) + N_half)

  # Generate time ranges
  times_group0 = sort(rexp(N_half, rate = 0.3))
  times_group1 = sort(rexp(N_half, rate = 0.3)) + max(times_group0)

  # Events with some noise
  # mostly 1s
  event_group0 = sample(c(0, 1, 2), size = N_half, replace = TRUE, prob = c(0.2, 0.6, 0.2))
  # mostly 2s
  event_group1 = sample(c(0, 1, 2), size = N_half, replace = TRUE, prob = c(0.3, 0.1, 0.6))

  data = data.table::data.table(
    x = rep(0:1, each = N_half),
    time = c(times_group0, times_group1),
    event = c(event_group0, event_group1)
  )
  tasks$sanity = mlr3proba::TaskCompRisks$new("sanity", mlr3::as_data_backend(data),
                                              time = "time", event = "event")

  tasks
}
registerS3method("generate_tasks", "LearnerCompRisks", generate_tasks.LearnerCompRisks)

sanity_check.PredictionCompRisks = function(prediction, ...) {
  # sanity check discrimination
  prediction$score() >= 0.5
}
registerS3method("sanity_check", "PredictionCompRisks", sanity_check.PredictionCompRisks)
