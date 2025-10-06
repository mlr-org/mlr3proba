#' <% learn = get(fullname)$new() %>
#' <% shortname = learn$id %>
#'
#' @include LearnerDens.R
#' @title <%=title%> Density Estimator
#' @name <%= paste("mlr_learners", shortname, sep = "_")%>
#' @description
#' Calls <%=caller%> and the result is coerced to a [distr6::Distribution].
#'
#' @section Dictionary:
#' This [Learner][mlr3::Learner] can be instantiated via the [dictionary][mlr3misc::Dictionary]
#' [mlr_learners][mlr3::mlr_learners] or with the associated sugar function [lrn()][mlr3::lrn]:
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
#' * Properties: `<%= format_types(learn$properties) %>`
#' * Packages: <%= paste0("\\CRANpkg{", learn$packages, "}") %>
#'
#' @family density estimators
#' @template seealso_learner
