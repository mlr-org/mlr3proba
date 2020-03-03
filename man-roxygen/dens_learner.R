#' <% learn = get(fullname)$new() %>
#' <% shortname = learn$id %>
#'
#' @include LearnerDens.R
#' @title <%=title%> Density Estimator
#' @usage NULL
#' @format [R6::R6Class()] inheriting from [LearnerDens].
#' @aliases <%= paste("mlr_learners", shortname, sep = "_")%>
#' @description
#' Calls <%=caller%> and the result is coerced to a [distr6::Distribution].
#'
#'
#' @section Construction:
#' ```
#' <%=fullname%>$new()
#' mlr_learners$get("<%=shortname%>")
#' lrn("<%=shortname%>")
#' ```
#'
#' @section Meta Information:
#' * Type: "dens"
#' * Predict Types: `<%= format_types(learn$predict_types) %>`
#' * Feature Types: `<%= format_types(learn$feature_types) %>`
#' * Packages: <%= paste0("\\CRANpkg{", learn$packages, "}") %>
#'
#' @family density estimators
#' @template seealso_learner
