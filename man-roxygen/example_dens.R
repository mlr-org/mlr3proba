#' <% learner = get(fullname)$new() %>
#' <% id = learner$id %>
#'
#' @examplesIf mlr3misc::require_namespaces(learner$packages, quietly = TRUE)
#' # Define the Learner
#' <%= sprintf("learner = lrn(\"%s\")", id)%>
#' print(learner)
#'
#' # Define a Task
#' task = tsk("faithful")
#'
#' # Create train and test set
#' ids = partition(task)
#'
#' # Train the learner on the training ids
#' learner$train(task, row_ids = ids$train)
#'
#' print(learner$model)
#'
#' # Make predictions for the test rows
#' predictions = learner$predict(task, row_ids = ids$test)
#'
#' # Score the predictions
#' predictions$score()
