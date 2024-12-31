#' <% measure = suppressWarnings(get(fullname)$new()) %>
#'
#' @examples
#' library(mlr3)
#'
#' # Define a survival Task
#' task = tsk("lung")
#'
#' # Create train and test set
#' part = partition(task)
#'
#' # Train Cox learner on the train set
#' cox = lrn("surv.coxph")
#' cox$train(task, row_ids = part$train)
#'
#' # Make predictions for the test set
#' p = cox$predict(task, row_ids = part$test)
#'
#' # Integrated AUC score
#' p$score(msr("<%=measure$id%>"), task = task,
#'         train_set = part$train, learner = cox)
#'
#' # AUC at specific time point
#' p$score(msr("<%=measure$id%>", times = 600), task = task,
#'         train_set = part$train, learner = cox)
#'
#' # Integrated AUC at specific time points
#' p$score(msr("<%=measure$id%>", times = c(100, 200, 300, 400, 500)),
#'         task = task, train_set = part$train, learner = cox)
#'
