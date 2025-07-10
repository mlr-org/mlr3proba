#' @examplesIf mlr3misc::require_namespaces("riskRegression", quietly = TRUE)
#' library(mlr3)
#'
#' # Define the Learner
#' learner = lrn("cmprsk.aalen")
#' learner
#'
#' # Define a Task
#' task = tsk("pbc")
#'
#' # Stratification based on event
#' task$set_col_roles(cols = "status", add_to = "stratum")
#'
#' # Create train and test set
#' part = partition(task)
#'
#' # Train the learner on the training set
#' learner$train(task, row_ids = part$train)
#' learner$model
#'
#' # Make predictions for the test set
#' predictions = learner$predict(task, row_ids = part$test)
#' predictions
#'
#' # Score the predictions
#' <%= if(msr_id == "auc" || msr_id == "all") "
#' # AUC(t = 100), weighted mean score across causes (default)
#' predictions$score(msr(\"cmprsk.auc\", cause = \"mean\", time_horizon = 100))
#'
#' # AUC(t = 100), 1st cause
#' predictions$score(msr(\"cmprsk.auc\", cause = 1, time_horizon = 100))
#'
#' # AUC(t = 100), 2nd cause
#' predictions$score(msr(\"cmprsk.auc\", cause = 2, time_horizon = 100))
#' " %>
#'
