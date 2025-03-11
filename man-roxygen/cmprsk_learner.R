#' <% learner = get(fullname)$new() %>
#' <% shortname = learner$id %>
#'
#' @name <%= paste("mlr_learners", shortname, sep = "_")%>
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
#' `r paste(mlr3misc::rd_info(mlr3::lrn("<%= id %>")), collapse = "\n")`
#' @md
#'
#' @section Parameters:
#' `r paste(mlr3misc::rd_info(mlr3::lrn("<%= id %>")$param_set), collapse = "\n")`
#' @md
#'
#' @family competing risk learners
#' @template seealso_learner
