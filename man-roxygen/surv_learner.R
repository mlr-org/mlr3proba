#' <% crank_distr_str = "* `crank` is predicted as the expectation of the survival distribution, `distr`" %>
#' <% crank_lp_str = "* `crank` is identical to `lp`" %>
#' <% crank_str = "* crank is predicted " %>
#' <% lp_str = "* lp is predicted " %>
#' <% distr_str = "* distr is predicted " %>
#'
#' <% learn = get(fullname)$new() %>
#' <% shortname = learn$id %>
#'
#'
#' @include LearnerSurv.R
#' @title <%=title%> Survival Learner
#' @name <%= paste("mlr_learners", shortname, sep = "_")%>
#' @description
#' Calls <%=caller%>.
#'
#' <%= if(exists("lp")) paste0(lp_str, lp) %>
#' <%= if(exists("distr")) paste0(distr_str, distr) %>
#' <%= if(exists("crank")) paste0(crank_str, crank) %>
#' <%= if(!exists("crank") & exists("lp")) crank_lp_str %>
#' <%= if(!exists("crank") & !exists("lp")) crank_distr_str %>
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
#' * Type: "surv"
#' * Predict Types: `<%= format_types(learn$predict_types) %>`
#' * Feature Types: `<%= format_types(learn$feature_types) %>`
#' * Properties: `<%= format_types(learn$properties) %>`
#' * Packages: <%= paste0("\\CRANpkg{", learn$packages, "}") %>
#'
#' @family survival learners
#' @template seealso_learner
