#' <% meas = get(fullname)$new() %>
#' <% shortname = meas$id %>
#'
#' @include MeasureDens.R
#' @title <%=title%> Density Measure
#' @name <%= paste("mlr_measures", shortname, sep = "_")%>
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
#' @section Parameters:
#' `r mlr3misc::rd_info(mlr3::msr("<%= shortname %>")$param_set)`
#' @md
#'
#' @section Meta Information:
#' * Type: `"density"`
#' * Range: <%= format_range(meas$range) %>
#' * Minimize: `<%=meas$minimize%>`
#' * Required prediction: `<%=meas$predict_type%>`
#'
#' @family density measures
#' @template seealso_measure
