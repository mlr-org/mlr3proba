#' @description
#' A <%=task_type%> task for the [<%=data%>]<%=if(exists("data_pkg"))paste0("[", data_pkg, "::", data, "]")%> data set.
#'
#' @format [R6::R6Class] inheriting from [<%=paste0("Task",type)%>].
#'
#' @section Dictionary:
#' This [Task][mlr3::Task] can be instantiated via the [dictionary][mlr3misc::Dictionary] [mlr_tasks][mlr3::mlr_tasks] or with the associated sugar function [tsk()][mlr3::tsk()]:
#' ```
#' mlr_tasks$get("<%= id %>")
#' tsk("<%= id %>")
#' ```
#'
#' @section Meta Information:
#' `r mlr3misc::rd_info(mlr3::tsk("<%= id %>"))`
#' @md
