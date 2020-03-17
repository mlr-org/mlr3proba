#' <% meas = get(fullname)$new() %>
#' <% shortname = meas$id %>
#'
#' @include MeasureSurv.R
#' @title <%=title%> Survival Measure
#' @aliases <%= paste("mlr_measures", shortname, sep = "_")%>
#'
#' @section Dictionary:
#' This [Measure][mlr3::Measure] can be instantiated via the [dictionary][mlr3misc::Dictionary]
#' [mlr_measures][mlr3::mlr_measures] or with the associated sugar function [msr()][mlr3::msr]:
#' ```
#' <%=fullname%>$new(<%= if(exists("pars")) pars %>)
#' mlr_measures$get("<%=shortname%>")
#' msr("<%=shortname%>")
#' ```
#'
#' @section Meta Information:
#' * Type: `"surv"`
#' * Range: <%= format_range(meas$range) %>
#' * Minimize: `<%=meas$minimize%>`
#' * Required prediction: `<%=meas$predict_type%>`
#'
#' @family survival measures
#' @template seealso_measure
