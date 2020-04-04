expect_task_dens = function(task) {
  expect_is(task, "TaskDens")
  expect_task(task)
  expect_is(task$truth(), "numeric")

  f = task$formula()
  expect_formula(f)
  expect_set_equal(mlr3misc::extract_vars(f)$lhs, task$target_names)
}

generate_tasks.LearnerDens = function(learner, N = 30L) {
  target = rnorm(N)
  data = cbind(data.table::data.table(target = target), generate_data(learner, N))
  task = TaskDens$new("proto", mlr3::as_data_backend(data), target = "target")

  tasks = generate_generic_tasks(learner, task)

  # generate sanity task
  with_seed(100, {
    data = data.table::data.table(x = c(rnorm(100, 0, 1), rnorm(100, 10, 1)), y = rnorm(200))
    data$unimportant = runif(nrow(data))
  })
  task = mlr3misc::set_names(list(TaskDens$new("sanity", mlr3::as_data_backend(data), target = "y")), "sanity")
  tasks = c(tasks, task)
}

registerS3method("generate_tasks", "LearnerDens", generate_tasks.LearnerDens)

sanity_check.PredictionDens = function(prediction) {
  prediction$score() >= -50 & prediction$score() <= 50
}
registerS3method("sanity_check", "PredictionDens", sanity_check.PredictionDens)

expect_prediction_dens = function(p) {
  expect_prediction(p)
  expect_is(p, "PredictionDens")
}

expect_task_surv = function(task) {
  expect_is(task, "TaskSurv")
  expect_task(task)
  expect_task_supervised(task)
  expect_is(task$truth(), "Surv")

  f = task$formula()
  expect_formula(f)
  expect_set_equal(mlr3misc::extract_vars(f)$lhs, task$target_names)
  #  expect_is(task$survfit(), "survfit")
}

generate_tasks.LearnerSurv = function(learner, N = 20L) {

  real.time = 1 + rexp(N, rate = 2) * 20
  cens.time = 1 + rexp(N, rate = 3) * 20
  status = ifelse(real.time <= cens.time, 1L, 0L)
  obs.time = ifelse(real.time <= cens.time, real.time, cens.time)

  data = cbind(data.table::data.table(time = obs.time, event = status), generate_data(learner, N))
  task = TaskSurv$new("proto", mlr3::as_data_backend(data), time = "time", event = "event")
  tasks = generate_generic_tasks(learner, task)

  # generate sanity task
  set.seed(100)
  data = data.table::data.table(time = obs.time, event = status, x1 = real.time + rnorm(N, sd = 0.1))
  data$unimportant = runif(nrow(data), 0, 20)
  task = mlr3misc::set_names(list(TaskSurv$new("sanity", mlr3::as_data_backend(data), time = "time", event = "event")), "sanity")
  tasks = c(tasks, task)
}
registerS3method("generate_tasks", "LearnerSurv", generate_tasks.LearnerSurv)

sanity_check.PredictionSurv = function(prediction) {
  prediction$score() >= 0
}
registerS3method("sanity_check", "PredictionSurv", sanity_check.PredictionSurv)

expect_prediction_surv = function(p) {
  checkmate::expect_r6(p, "Prediction", public = c("row_ids", "truth", "predict_types"))
  #testthat::expect_output(print(p), "^<Prediction")
  checkmate::expect_data_table(data.table::as.data.table(p), nrows  = length(p$row_ids))
  checkmate::expect_atomic_vector(p$missing)
  expect_is(p, "PredictionSurv")
}

run_experiment = function(task, learner, seed = NULL) {
  err = function(info, ...) {
    info = sprintf(info, ...)
    list(
      ok = FALSE, seed = seed,
      task = task, learner = learner, prediction = prediction, score = score,
      error = sprintf("[%s] learner '%s' on task '%s' failed: %s",
                      stage, learner$id, task$id, info)
    )
  }

  if (is.null(seed)) {
    seed = sample.int(floor(.Machine$integer.max / 2L), 1L)
  }

  old_seed = get0(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
  if (is.null(old_seed)) {
    runif(1L)
    old_seed = get0(".Random.seed", globalenv(), mode = "integer", inherits = FALSE)
  }
  on.exit(assign(".Random.seed", old_seed, globalenv()), add = TRUE)

  set.seed(seed)

  task = mlr3::assert_task(mlr3::as_task(task))
  learner = mlr3::assert_learner(mlr3::as_learner(learner, clone = TRUE))
  mlr3::assert_learnable(task, learner)
  prediction = NULL
  score = NULL
  learner$encapsulate = c(train = "evaluate", predict = "evaluate")

  stage = "train()"
  ok = try(learner$train(task), silent = TRUE)
  if (inherits(ok, "try-error"))
    return(err(as.character(ok)))
  log = learner$log[stage == "train"]
  if ("error" %in% log$class)
    return(err("train log has errors: %s", mlr3misc::str_collapse(log[class == "error", msg])))
  if (is.null(learner$model))
    return(err("model is NULL"))

  stage = "predict()"

  if (grepl("reordered", task$id)) {
    task$col_roles$feature = rev(task$col_roles$feature)
  }

  prediction = try(learner$predict(task), silent = TRUE)
  if (inherits(ok, "try-error"))
    return(err(as.character(ok)))
  log = learner$log[stage == "predict"]
  if ("error" %in% log$class)
    return(err("predict log has errors: %s", mlr3misc::str_collapse(log[class == "error", msg])))
  msg = checkmate::check_class(prediction, "Prediction")
  if (!isTRUE(msg))
    return(err(msg))
  if (prediction$task_type != learner$task_type)
    return(err("learner and prediction have different task_type"))

  expected = mlr3::mlr_reflections$learner_predict_types[[learner$task_type]][[learner$predict_type]]
  msg = checkmate::check_subset(prediction$predict_types, expected, empty.ok = FALSE)
  if (!isTRUE(msg))
    return(err(msg))

  msg = checkmate::check_subset(learner$predict_type, learner$predict_types)
  if (!isTRUE(msg))
    return(err(msg))

  stage = "score()"
  score = try(prediction$score(mlr3::default_measures(learner$task_type)), silent = TRUE)
  if (inherits(score, "try-error"))
    return(err(as.character(score)))
  msg = checkmate::check_numeric(score, any.missing = FALSE)
  if (!isTRUE(msg))
    return(err(msg))

  # run sanity check on sanity task
  if (startsWith(task$id, "sanity") && !sanity_check(prediction)) {
    return(err("sanity check failed"))
  }

  if (startsWith(task$id, "feat_all")) {
    if ("importance" %in% learner$properties) {
      importance = learner$importance()
      msg = checkmate::check_numeric(rev(importance), any.missing = FALSE, min.len = 1L, sorted = TRUE)
      if (!isTRUE(msg))
        return(err(msg))
      msg = checkmate::check_names(names(importance), subset.of = task$feature_names)
      if (!isTRUE(msg))
        return(err("Names of returned importance scores do not match task names: %s", mlr3misc::str_collapse(names(importance))))
      if ("unimportant" %in% head(names(importance), 1L))
        return(err("unimportant feature is important"))
    }

    if ("selected_features" %in% learner$properties) {
      selected = learner$selected_features()
      msg = checkmate::check_subset(selected, task$feature_names)
      if (!isTRUE(msg))
        return(err(msg))
    }

    if ("oob_error" %in% learner$properties) {
      err = learner$oob_error()
      msg = checkmate::check_number(err)
      if (!isTRUE(msg))
        return(err(msg))
    }
  }

  return(list(ok = TRUE, learner = learner, prediction = prediction, error = character(), seed = seed))
}

run_autotest = function(learner, N = 30L, exclude = NULL) {
  learner = learner$clone(deep = TRUE)
  id = learner$id
  tasks = generate_tasks(learner, N = N)
  if (!is.null(exclude))
    tasks = tasks[!grepl(exclude, names(tasks))]

  for (task in tasks) {
    predict_type = learner$predict_type
    learner$id = sprintf("%s:%s", id, predict_type)
    learner$predict_type = predict_type

    run = run_experiment(task, learner)
    if (!run$ok) {
      return(run)
    }

    # re-run task with same seed for feat_all
    if (startsWith(task$id, "feat_all")) {
      repeated_run = run_experiment(task, learner, seed = run$seed)

      if (!repeated_run$ok) {
        return(repeated_run)
      }


      x = try(run$prediction$distr, silent = TRUE)
      x = if(is.null(x)) FALSE else class(x)[1] != "try-error"
      if(x){
        if(inherits(run$prediction$distr,"VectorDistribution")){
          if (!all.equal(as.data.table(run$prediction)[,-c("distr")],
                         as.data.table(repeated_run$prediction)[,-c("distr")])) {
            run$ok = FALSE
            run$error = sprintf("Different results for replicated runs using fixed seed %i", seed)
            return(run)
          }
        } else {
          if (!all.equal(as.data.table(run$prediction), as.data.table(repeated_run$prediction))) {
            run$ok = FALSE
            run$error = sprintf("Different results for replicated runs using fixed seed %i", seed)
            return(run)
          }
        }
      } else {
        if (!all.equal(as.data.table(run$prediction), as.data.table(repeated_run$prediction))) {
          run$ok = FALSE
          run$error = sprintf("Different results for replicated runs using fixed seed %i", seed)
          return(run)
        }
      }
    }
  }

  return(TRUE)
}
