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
#' # <%=improper_id%>, G(t) calculated using the test set
#' p$score(msr("<%=measure$id%>"))
#'
#' # <%=improper_id%>, G(t) calculated using the train set (always recommended)
#' p$score(msr("<%=measure$id%>"), task = task, train_set = part$train)
#'
#' # <%=improper_id%>, ERV score (comparing with KM baseline)
#' p$score(msr("<%=measure$id%>", ERV = TRUE), task = task, train_set = part$train)
#'
#' # <%=improper_id%> at specific time point
#' p$score(msr("<%=measure$id%>", times = 365), task = task, train_set = part$train)
#'
#' # <%=improper_id%> at multiple time points (integrated)
#' p$score(msr("<%=measure$id%>", times = c(125, 365, 450), integrated = TRUE),
#'         task = task, train_set = part$train)
#'
#' # <%=improper_id%>, use time cutoff
#' p$score(msr("<%=measure$id%>", t_max = 700), task = task, train_set = part$train)
#'
#' # <%=improper_id%>, use time cutoff and also remove observations
#' p$score(msr("<%=measure$id%>", t_max = 700, remove_obs = TRUE),
#'         task = task, train_set = part$train)
#'
#' # <%=improper_id%>, use time cutoff corresponding to specific proportion of censoring on the test set
#' p$score(msr("<%=measure$id%>", p_max = 0.8), task = task, train_set = part$train)
#'
#' # <%=proper_id%>, G(t) calculated using the train set
#' p$score(msr("<%=measure$id%>", proper = TRUE), task = task, train_set = part$train)
#'
