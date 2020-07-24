#' @title <%=title%> Pipeline
#' @description Wrapper around [<%=pipeop>%] to simplify [Graph][mlr3pipelines::Graph] creation.
#' @param graph_learner :: `logical(1)`\cr
#' If `TRUE` returns wraps the [Graph][mlr3pipelines::Graph] as a
#' [GraphLearner][mlr3pipelines::GraphLearner] otherwise (default) returns as a `Graph`.
#' @return [mlr3pipelines::Graph] or [mlr3pipelines::GraphLearner]
#' @family pipelines
#' @examples
#' \dontrun{
#' library("mlr3")
#'
#' task = tgen("simsurv")$generate(5)
#' avg = surv_averager(
#'   learners = lrns(c("surv.kaplan", "surv.coxph")),
#'   param_vals = list(weights = c(0.1, 0.9))
#'  )
#' avg$train(task)$predict(task)
#' }
#' @export
