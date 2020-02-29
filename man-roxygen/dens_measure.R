#'
#' <% eps_str = "* `eps` :: `numeric(1)` \\cr Very small number to set zero-valued predicted probabilities to, in order to prevent errors in log(0) calculation." %>
#'
#' <% meas = get(fullname)$new() %>
#' <% shortname = meas$id %>
#'
#' @include MeasureDens.R
#' @title <%=title%> Density Measure
#' @usage NULL
#' @format [R6::R6Class()] inheriting from <%=inherit%>.
#' @aliases <%= paste("mlr_measures", shortname, sep = "_")%>
#' @description
#'
#' @section Construction:
#' ```
#' <%=fullname%>$new(<%= if(exists("pars")) pars %>)
#' mlr_measures$get("<%=shortname%>")
#' msr("<%=shortname%>")
#' ```
#'
#' <%= if(exists("eps_par")) paste(eps_str, "\\cr") %>
#'
#' @section Meta Information:
#' * Type: `"density"`
#' * Range: <%= format_range(meas$range) %>
#' * Minimize: `<%=meas$minimize%>`
#' * Required prediction: `<%=meas$predict_type%>`
#'
#' @section Fields:
#' See [MeasureDens], as well as all variables passed to the constructor.
#'
#' @family density measures
#' @template seealso_measure
