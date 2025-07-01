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
#' # <%=id%>, G(t) calculated using the test set
#' p$score(msr("<%=measure$id%>"))
#'
#' # <%=id%>, G(t) calculated using the train set (always recommended)
#' p$score(msr("<%=measure$id%>"), task = task, train_set = part$train)
#'
#' # <%=id%>, ERV score (comparing with KM baseline)
#' p$score(msr("<%=measure$id%>", ERV = TRUE), task = task, train_set = part$train)
#'
#' # <%=id%> at specific time point
#' p$score(msr("<%=measure$id%>", times = 365), task = task, train_set = part$train)
#'
#' # <%=id%> at multiple time points (integrated)
#' p$score(msr("<%=measure$id%>", times = c(125, 365, 450), integrated = TRUE),
#'         task = task, train_set = part$train)
#'
#' # <%=id%>, use time cutoff
#' p$score(msr("<%=measure$id%>", t_max = 700), task = task, train_set = part$train)
#'
#' # <%=id%>, use time cutoff corresponding to specific proportion of censoring on the test set
#' p$score(msr("<%=measure$id%>", p_max = 0.8), task = task, train_set = part$train)
#'
