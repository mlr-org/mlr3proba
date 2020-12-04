generate_tasks.LearnerDens = function(learner, N = 30L, ...) { # nolint
  task = mlr3proba::TaskDens$new("proto", rnorm(N))

  tasks = list()
  tasks$feat_single_integer = mlr3proba::TaskDens$new("feat_single_integer",
                                           data.frame(integer = rbinom(N, 10, 0.5)))
  tasks$feat_single_numeric = mlr3proba::TaskDens$new("feat_single_numeric", data.frame(numeric = rnorm(N)))
  tasks$weights = mlr3proba::TaskDens$new("feat_single_numeric", data.frame(x = rnorm(N), weights = runif(N)))

  # generate sanity task
  data = with_seed(100, rnorm(1000, 10, 1))
  tasks$sanity = mlr3proba::TaskDens$new("sanity", data)
  tasks$sanity_reordered = tasks$sanity$clone(deep = TRUE)

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
  tasks$sanity_reordered = tasks$sanity$clone(deep = TRUE)
  tasks$sanity_reordered$id = "sanity_reordered"

  tasks
}
registerS3method("generate_tasks", "LearnerSurv", generate_tasks.LearnerSurv)

sanity_check.PredictionSurv = function(prediction, ...) { # nolint
  # sanity check discrimination
  x = prediction$score() >= 0.6

  if ("lp" %in% prediction$predict_types) {
    # crank should equal lp if available
    x = x & all(all.equal(as.numeric(prediction$lp),
                          as.numeric(prediction$crank)) == TRUE)
  } else if ("response" %in% prediction$predict_types) {
    # otherwise crank should equal -response if available
    x = x & all(all.equal(-as.numeric(prediction$response),
                          as.numeric(prediction$crank)) == TRUE)
  } else if ("distr" %in% prediction$predict_types) {
    # try faster method first
    mean = suppressMessages(as.numeric(prediction$distr$mean(cubature = FALSE)))
    if (any(is.nan(mean))) {
      mean = suppressMessages(as.numeric(prediction$distr$mean(cubature = TRUE)))
    }
    if(!any(is.nan(mean))) {
      # otherwise crank should equal -distr$mean if available
      x = x & all(all.equal(-mean, as.numeric(prediction$crank)) == TRUE)
    }
  }

  x
}
registerS3method("sanity_check", "PredictionSurv", sanity_check.PredictionSurv)
