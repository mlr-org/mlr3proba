#' <% meas = get(fullname)$new() %>
#' <% shortname = meas$id %>
#'
#' @include MeasureRegr.R
#' @title <%=title%> Regression Measure
#' @name <%= paste("mlr_measures", shortname, sep = "_")%>
#'
#' @section Parameters:
#' `r mlr3misc::rd_info(mlr3::msr("<%= shortname %>")$param_set)`
#' @md
#'
#' @section Meta Information:
#' * Type: `"regr"`
#' * Range: <%= format_range(meas$range) %>
#' * Minimize: `<%=meas$minimize%>`
#' * Required prediction: `<%=meas$predict_type%>`
#'
#' @family regression measures
#' @template seealso_measure
