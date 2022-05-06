.scoring_rule_erv = function(measure, prediction, task, train_set) {
  if (is.null(task) || is.null(train_set)) {
    stop("'task' and 'train_set' are required if 'ERV' is 'TRUE'")
  }

  ps = measure$param_set$values
  if (!is.null(ps$se) && ps$se) {
    stop("Only one of `ERV` and `se` can be TRUE")
  } else {
    measure$param_set$values$ERV = FALSE
    # compute score for the learner
    learner_score = measure$score(prediction, task = task,
                                  train_set = train_set)

    # compute score for the baseline (Kaplan-Meier)
    km = lrn("surv.kaplan")$train(task = task, row_ids = train_set)
    km = km$predict(as_task_surv(data.frame(
      as.matrix(prediction$truth)), event = "status"))
    base_score = measure$score(km, task = task, train_set = train_set)

    measure$param_set$values$ERV = TRUE

    # return percentage decrease
    1 - (learner_score / base_score)
  }
}
