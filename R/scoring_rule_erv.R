.scoring_rule_erv = function(measure, prediction, task, train_set) {
  if (is.null(task) || is.null(train_set)) {
    stop("'task' and 'train_set' are required if 'ERV' is 'TRUE'")
  }

  measure$param_set$set_values(ERV = FALSE)
  # compute score for the learner
  learner_score = measure$score(prediction, task = task, train_set = train_set)

  # compute score for the baseline (Kaplan-Meier)
  # train KM
  km = lrn("surv.kaplan")$train(task = task, row_ids = train_set)
  # predict KM on the test set (= not train ids)
  test_set = setdiff(task$row_ids, train_set)
  km_pred = km$predict(task, row_ids = test_set)
  base_score = measure$score(km_pred, task = task, train_set = train_set)

  measure$param_set$set_values(ERV = TRUE)

  # return percentage decrease
  1 - (learner_score / base_score)
}
