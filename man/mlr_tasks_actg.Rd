% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/TaskSurv_zzz.R
\name{mlr_tasks_actg}
\alias{mlr_tasks_actg}
\title{ACTG 320 Survival Task}
\format{
\link[R6:R6Class]{R6::R6Class} inheriting from \link{TaskSurv}.
}
\description{
A survival task for the \link{actg} data set.
}
\details{
Column "sex" has been renamed to "sexF" and "censor" has been renamed to "status".
Columns "id", "time_d", and "censor_d" have been removed so target is time to AIDS diagnosis
or death.
}
\section{Construction}{


\if{html}{\out{<div class="sourceCode">}}\preformatted{mlr3::mlr_tasks$get("actg")
mlr3::tsk("actg")
}\if{html}{\out{</div>}}
}

