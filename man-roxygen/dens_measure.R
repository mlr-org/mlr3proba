#' <% meas = get(fullname)$new() %>
#' <% shortname = meas$id %>
#'
#' @include MeasureDens.R
#' @title <%=title%> Density Measure
#' @aliases <%= paste("mlr_measures", shortname, sep = "_")%>
#'
#' @section Meta Information:
#' * Type: `"density"`
#' * Range: <%= format_range(meas$range) %>
#' * Minimize: `<%=meas$minimize%>`
#' * Required prediction: `<%=meas$predict_type%>`
#'
#' @family density measures
#' @template seealso_measure
