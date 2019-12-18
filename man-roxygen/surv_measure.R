#'
#' <% int_str = "* `integrated` :: `logical(1)` \\cr If `TRUE` (default), returns the integrated score; otherwise, not integrated." %>
#' <% times_str = "* `times` :: `vector()` \\cr If `integrate == TRUE` then a vector of time-points over which to integrate the score. If `integrate == FALSE` then a single time point at which to return the score." %>
#  <% thresh_str = "* `lp_thresh` :: `numeric(1)` \\cr Determines where to threshold the linear predictor for calculating the TPR/TNR." %>
#' <% type_str = "* `type` :: `character(1)` \\cr Determines the type of score, one of: `'cumulative'`, `'incident'`. Default `'incident'`." %>
#' <% eps_str = "* `eps` :: `numeric(1)` \\cr Very small number to set zero-valued predicted probabilities to, in order to prevent errors in log(0) calculation." %>
#'
#' <% meas = get(fullname)$new() %>
#' <% shortname = meas$id %>
#'
#' @include MeasureSurv.R
#' @title <%=title%> Survival Measure
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
#' <%= if(exists("int_par")) paste(int_str, "\\cr") %>
#' <%= if(exists("times_par")) paste(times_str, "\\cr") %>
#' <%= if(exists("thresh_par")) paste(thresh_str, "\\cr") %>
#' <%= if(exists("type_par")) paste(type_str, "\\cr") %>
#' <%= if(exists("eps_par")) paste(eps_str, "\\cr") %>
#'
#' @section Meta Information:
#' * Type: `"surv"`
#' * Range: <%= format_range(meas$range) %>
#' * Minimize: `<%=meas$minimize%>`
#' * Required prediction: `<%=meas$predict_type%>`
#'
#' @section Fields:
#' See [MeasureSurv], as well as all variables passed to the constructor.
#'
#' @family survival measures
#' @template seealso_measure
