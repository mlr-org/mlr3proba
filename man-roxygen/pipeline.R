#' @name <%= paste0("mlr_graphs_", id)%>
#' @title <%=title%> Pipeline
#' @description Wrapper around <%=pipeop%> to simplify [Graph][mlr3pipelines::Graph] creation.
#' @param graph_learner (`logical(1)`)\cr
#' If `TRUE` returns wraps the [Graph][mlr3pipelines::Graph] as a
#' [GraphLearner][mlr3pipelines::GraphLearner] otherwise (default) returns as a `Graph`.
#' @return [mlr3pipelines::Graph] or [mlr3pipelines::GraphLearner]
#' @family pipelines
#' @export
