lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

expect_task_dens = function(task) {
  expect_is(task, "TaskDens")
  expect_task(task)
  expect_is(task$truth(), "numeric")

  f = task$formula()
  expect_formula(f)
  expect_set_equal(extract_vars(f)$lhs, task$target_names)
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
  expect_set_equal(extract_vars(f)$lhs, task$target_names)
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

# see mlr3pipelines
expect_pipeop_class = function(poclass, constargs = list()) {
  skip_on_cran()
  po = do.call(poclass$new, constargs)

  expect_pipeop(po)

  poclone = po$clone(deep = TRUE)
  expect_deep_clone(po, poclone)

  in_nop = rep(list(NO_OP), po$innum)
  in_nonnop = rep(list(NULL), po$innum)
  out_nop = rep(list(NO_OP), po$outnum)
  names(out_nop) = po$output$name

  expect_false(po$is_trained)
  expect_equal(po$train(in_nop), out_nop)
  expect_equal(po$predict(in_nop), out_nop)
  expect_true(is_noop(po$state))
  expect_true(po$is_trained)

  expect_error(po$predict(in_nonnop), "Pipeop .* got NO_OP during train")

  # check again with no_op-trained PO
  expect_pipeop(po)
  poclone = po$clone(deep = TRUE)
  expect_deep_clone(po, poclone)

}

# see mlr3pipelines
expect_pipeop = function(po) {

  label = sprintf("pipeop '%s'", po$id)
  expect_class(po, "PipeOp", label = label)
  expect_string(po$id, label = label)
  expect_class(po$param_set, "ParamSet", label = label)
  expect_list(po$param_set$values, names = "unique", label = label)
  expect_flag(po$is_trained, label = label)
  expect_output(print(po), "PipeOp:", label = label)
  expect_character(po$packages, any.missing = FALSE, unique = TRUE, label = label)
  expect_function(po$train, nargs = 1)
  expect_function(po$predict, nargs = 1)
  expect_function(po$train_internal, nargs = 1)
  expect_function(po$predict, nargs = 1)
  expect_function(po$predict_internal, nargs = 1)
  expect_data_table(po$input, any.missing = FALSE)
  expect_names(names(po$input), permutation.of = c("name", "train", "predict"))
  expect_data_table(po$output, any.missing = FALSE)
  expect_names(names(po$output), permutation.of = c("name", "train", "predict"))
  expect_int(po$innum, lower = 1)
  expect_int(po$outnum, lower = 1)
  # at least one of "train" or "predict" must be in every parameter's tag
  testthat::expect_true(every(po$param_set$tags, function(x) length(intersect(c("train", "predict"), x)) > 0))

}

# see mlr3pipelines
expect_deep_clone = function(one, two) {
  # is equal
  expect_equal(one, two)
  visited = new.env()
  visited_b = new.env()
  expect_references_differ = function(a, b, path) {

    force(path)

    # don't go in circles
    addr_a = data.table::address(a)
    addr_b = data.table::address(b)
    if (!is.null(visited[[addr_a]])) {
      return(invisible(NULL))
    }
    visited[[addr_a]] = path
    visited_b[[addr_b]] = path

    # follow attributes, even for non-recursive objects
    if (utils::tail(path, 1) != "[attributes]" && !is.null(base::attributes(a))) {
      expect_references_differ(base::attributes(a), base::attributes(b), c(path, "[attributes]"))
    }

    # don't recurse if there is nowhere to go
    if (!base::is.recursive(a)) {
      return(invisible(NULL))
    }

    # check that environments differ
    if (base::is.environment(a)) {
      # some special environments
      if (identical(a, baseenv()) || identical(a, globalenv()) || identical(a, emptyenv())) {
        return(invisible(NULL))
      }
      if (length(path) > 1 && R6::is.R6(a) && "clone" %nin% names(a)) {
        return(invisible(NULL))  # don't check if smth is not cloneable
      }
      if (identical(utils::tail(path, 1), c("[element train_task] 'train_task'"))) {
        return(invisible(NULL))  # workaround for https://github.com/mlr-org/mlr3/issues/382
      }
      label = sprintf("Object addresses differ at path %s", paste0(path, collapse = "->"))
      expect_true(addr_a != addr_b, label = label)
      expect_null(visited_b[[addr_a]], label = label)
    }

    # recurse
    if (base::is.function(a)) {
      return(invisible(NULL))
      ## # maybe this is overdoing it
      ## expect_references_differ(base::formals(a), base::formals(b), c(path, "[function args]"))
      ## expect_references_differ(base::body(a), base::body(b), c(path, "[function body]"))
    }
    objnames = base::names(a)
    if (is.null(objnames) || anyDuplicated(objnames)) {
      index = seq_len(base::length(a))
    } else {
      index = objnames
      if (base::is.environment(a)) {
        index = Filter(function(x) !bindingIsActive(x, a), index)
      }
    }
    for (i in index) {
      if (utils::tail(path, 1) == "[attributes]" && i %in% c("srcref", "srcfile", ".Environment")) next
      expect_references_differ(base::`[[`(a, i), base::`[[`(b, i), c(path, sprintf("[element %s]%s", i,
                                                                                   if (!is.null(objnames)) sprintf(" '%s'", if (is.character(index)) i else objnames[[i]]) else "")))
    }
  }
  expect_references_differ(one, two, "ROOT")
}

# see mlr3pipelines
expect_shallow_clone = function(one, two) {
  expect_equal(one, two)
  if (base::is.environment(one)) {
    addr_a = data.table::address(one)
    addr_b = data.table::address(two)
    expect_true(addr_a != addr_b, label = "Objects are shallow clones")
  }
}
