#' <% meas = get(fullname)$new() %>
#' <% shortname = meas$id %>
#'
#' @include MeasureRegr.R
#' @title <%=title%> Regression Measure
#' @name <%= paste("mlr_measures", shortname, sep = "_")%>
#'
#' @section Meta Information:
#' * Type: `"regr"`
#' * Range: <%= format_range(meas$range) %>
#' * Minimize: `<%=meas$minimize%>`
#' * Required prediction: `<%=meas$predict_type%>`
#'
#' @family regression measures
#' @template seealso_measure
